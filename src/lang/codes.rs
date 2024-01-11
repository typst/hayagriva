use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref LANGUAGE_CODE_MAPPING: HashMap<&'static str, &'static str> =
        HashMap::from([
            ("english", "en"),
            ("german", "ge"),
            ("french", "fr"),
            ("russian", "ru"),
            ("italian", "it"),
            ("chinese", "cn"),
            ("japanese", "jp"),
            ("ukranian", "ua")
        ]);
}

pub fn get_mapping(s: &str) -> Option<&str> {
    return LANGUAGE_CODE_MAPPING.get(s).copied();
}
