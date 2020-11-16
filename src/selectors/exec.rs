use std::collections::HashMap;
use std::str::FromStr;

use crate::Entry;
use crate::types::EntryType;

use super::syntax::*;

fn is_mp(expr: &Expr) -> bool {
    let expr = if let Expr::Binary(bop) = expr {
        bop
    } else {
        return false;
    };

    expr.op.v == BinOp::MultiParent
}

fn flatten_mp(expr: &Expr) -> Vec<&Expr> {
    let expr = if let Expr::Binary(bop) = expr {
        bop
    } else {
        panic!("argument was not a binary expression");
    };

    if expr.op.v != BinOp::MultiParent {
        panic!("argument was not a binary expression");
    }

    let mut res = vec![];

    if is_mp(&expr.lhs.v) {
        res.extend(&flatten_mp(&expr.lhs.v))
    } else {
        res.push(expr.lhs.v.as_ref());
    }

    if is_mp(&expr.rhs.v) {
        res.extend(&flatten_mp(&expr.rhs.v))
    } else {
        res.push(expr.rhs.v.as_ref());
    }

    res
}

impl Expr {
    pub fn apply<'s>(&'s self, entry: &'s Entry, top: bool) -> Option<HashMap<String, &'s Entry>> {
        match self {
            Expr::Lit(Lit::Ident(i)) => {
                if Ok(entry.entry_type) == EntryType::from_str(&i.to_lowercase()) {
                    Some(HashMap::new())
                } else {
                    None
                }
            }
            Expr::Lit(Lit::Wildcard) => {
                Some(HashMap::new())
            }
            Expr::Binary(bop) => {
                match bop.op.v {
                    BinOp::Alternative => {
                        bop.lhs.v.apply(entry, top).or_else(|| bop.rhs.v.apply(entry, top))
                    }
                    BinOp::Ancestrage => {
                        bop.lhs.v.apply(entry, top).and_then(|mut r| {
                            let ps = entry.get_parents_opt().unwrap_or_default();
                            let r2 = bop.rhs.v.apply_any(ps);
                            if let Some(r2) = r2 {
                                r.extend(r2.0.into_iter());
                                Some(r)
                            } else {
                                None
                            }
                        })
                    }
                    BinOp::MultiParent => {
                        None
                    }
                }
            }
            Expr::Unary(uop) => {
                match uop.op.v {
                    UnOp::Neg => {
                        if let Some(_) = uop.expr.v.apply(entry, top) {
                            None
                        } else {
                            Some(HashMap::new())
                        }
                    }
                }
            }
            Expr::Tag(taop) => {
                match &taop.op.v {
                    TagOp::Bind(b) => {
                        taop.expr.v.apply(entry, top).map(|mut hm| {
                            hm.insert(b.to_string(), entry);
                            hm
                        })
                    }
                    TagOp::Attributes(args) => {
                        taop.expr.v.apply(entry, top).and_then(|hm| {
                            if args.iter().map(|arg| &arg.v).all(|arg| entry.get(arg.as_ref()).is_some()) {
                                Some(hm)
                            } else {
                                None
                            }
                        })
                    }
                }
            }
        }
    }

    fn apply_any<'s>(&'s self, entries: &'s [Entry]) -> Option<(HashMap<String, &'s Entry>, Vec<&'s Entry>)> {
        match self {
            Expr::Lit(Lit::Ident(_)) => {
                entries.iter().filter_map(|e| self.apply(e, false).map(|r| (r, vec![e]))).next()
            }
            Expr::Lit(Lit::Wildcard) => {
                if entries.len() > 0 {
                    Some((HashMap::new(), entries.iter().collect()))
                } else {
                    None
                }
            }
            Expr::Binary(bop) => {
                match bop.op.v {
                    BinOp::Alternative => {
                        bop.lhs.v.apply_any(entries).or_else(|| bop.rhs.v.apply_any(entries))
                    }
                    BinOp::Ancestrage => {
                        entries.iter().filter_map(|e| self.apply(e, false).map(|r| (r, vec![e]))).next()
                    }
                    BinOp::MultiParent => {
                        let unnest = flatten_mp(self);
                        let mut consumed = vec![];
                        let mut res = HashMap::new();
                        for spec in &unnest {
                            let mut item = None;
                            for (i, e) in entries.iter().enumerate() {
                                if consumed.contains(&i) {
                                    continue;
                                }

                                item = spec.apply(e, false).map(|v| (i, v));
                                if item.is_some() {
                                    break;
                                }
                            }

                            if let Some((index, hm)) = item {
                                res.extend(hm.into_iter());
                                consumed.push(index);
                            } else {
                                return None;
                            }
                        }
                        let mut es = vec![];

                        for i in consumed.into_iter() {
                            es.push(entries.get(i).unwrap());
                        }

                        Some((res, es))
                    }
                }
            }
            Expr::Unary(uop) => {
                match uop.op.v {
                    UnOp::Neg => {
                        if let Some(_) = uop.expr.v.apply_any(entries) {
                            None
                        } else {
                            Some((HashMap::new(), vec![]))
                        }
                    }
                }
            }
            Expr::Tag(taop) => {
                match &taop.op.v {
                    TagOp::Bind(b) => {
                        taop.expr.v.apply_any(entries).map(|(mut hm, es)| {
                            if es.len() == 1 {
                                hm.insert(b.to_string(), es.get(0).unwrap());
                            } else {
                                println!("WARN: Trying to bind to {} elems", es.len());
                            }
                            (hm, vec![])
                        })
                    }
                    TagOp::Attributes(args) => {
                        taop.expr.v.apply_any(entries).and_then(|(hm, es)| {
                            if es.len() > 0 {
                                if es.iter().any(|e| args.iter().map(|arg| &arg.v).all(|arg| e.get(arg.as_ref()).is_some())) {
                                    Some((hm, es))
                                } else {
                                    None
                                }
                            } else {
                                println!("WARN: Trying to check attrs on {} elems", es.len());
                                Some((hm, es))
                            }
                        })
                    }
                }
            }
        }
    }
}