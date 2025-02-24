use jsonschema;
use serde_json;
use serde_yaml;
use std::fs;
use std::path::Path;
use std::string::String;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn validate_schema() {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR"));

        let schema_string: String =
            fs::read_to_string(workspace.join("hayagriva.schema.json"))
                .expect("No file found at specified path.");

        let schema: serde_json::Value = serde_json::from_str(&schema_string)
            .expect("String read from file does not contain a valid JSON map.");

        assert!(jsonschema::meta::validate(&schema).is_ok())
    }

    #[test]
    fn validate_basic_yaml() {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR"));

        let schema_string: String =
            fs::read_to_string(workspace.join("hayagriva.schema.json"))
                .expect("No file found at specified path.");

        let schema: serde_json::Value = serde_json::from_str(&schema_string)
            .expect("String read from file does not contain a valid JSON map.");
        let basic_yml_string: String =
            fs::read_to_string(workspace.join("tests/data/basic.yml"))
                .expect("No file found at specified path.");

        let basic_yml: serde_json::Value = serde_yaml::from_str(&basic_yml_string)
            .expect("String read from file does not contain a valid YAML map.");

        assert!(jsonschema::validate(&schema, &basic_yml).is_ok());
    }
}
