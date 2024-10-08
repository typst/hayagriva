use hayagriva::citationberg::IndependentStyle;
use hayagriva::io::from_yaml_str;
use hayagriva::{
    archive::locales, BibliographyDriver, BibliographyRequest, CitationItem,
    CitationRequest,
};

#[test]
fn test_219() {
    let yaml = r#"
ITEM1:
    type: book
    author: Bumke, Joachim
    title: "Courtly Culture: Literature and Society in the High Middle Ages"
ITEM2:
    type: book
    author: Bumke, Joachim
    title: Höfische
"#;

    // Parse a bibliography
    let bib = from_yaml_str(yaml).unwrap();

    let style = IndependentStyle::from_xml(
        r#"
<style 
      xmlns="http://purl.org/net/xbiblio/csl"
      class="note"
      version="1.0">
  <info>
    <id />
    <title />
    <updated>2009-08-10T04:49:00+09:00</updated>
  </info>
  <citation>
    <layout>
      <text value="Bogus"/>
    </layout>
  </citation>
  <bibliography subsequent-author-substitute="-----">
    <layout>
      <group delimiter=", ">
        <names variable="author">
          <name form="short" and="text"/>
        </names>
        <text variable="title"/>
        <names variable="translator">
          <name form="short" and="text"/>
          <label form="short" prefix=" "/>
        </names>
      </group>
    </layout>
  </bibliography>
</style>
"#,
    )
    .unwrap();

    let item1 = bib.get("ITEM1").unwrap();
    let item2 = bib.get("ITEM2").unwrap();

    let locs = locales();

    let mut driver = BibliographyDriver::new();
    driver.citation(CitationRequest::from_items(
        vec![CitationItem::with_entry(item1)],
        &style,
        &locs,
    ));
    driver.citation(CitationRequest::from_items(
        vec![CitationItem::with_entry(item2)],
        &style,
        &locs,
    ));

    let result = driver.finish(BibliographyRequest {
        style: &style,
        locale: None,
        locale_files: &locs,
    });

    for i in result.bibliography.unwrap().items {
        eprintln!("{}", i.content.to_string());
    }
}
