use serde::{Deserialize, Serialize};

use hayagriva_core::Person;
use hayagriva_core::util::{deserialize_one_or_many, serialize_one_or_many};

/// A list of persons with a common role.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PersonsWithRoles {
    /// The persons.
    #[serde(serialize_with = "serialize_one_or_many")]
    #[serde(deserialize_with = "deserialize_one_or_many")]
    pub names: Vec<Person>,
    /// The role the persons had in the creation of the cited item.
    pub role: PersonRole,
}

impl PersonsWithRoles {
    /// Create a new list of persons with a common role.
    pub fn new(names: Vec<Person>, role: PersonRole) -> Self {
        Self { names, role }
    }
}

/// Specifies the role a group of persons had in the creation to the
/// cited item.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[non_exhaustive]
#[serde(rename_all = "kebab-case")]
pub enum PersonRole {
    /// Translated the work from a foreign language to the cited edition.
    Translator,
    /// Authored an afterword.
    Afterword,
    /// Authored an foreword.
    Foreword,
    /// Authored an introduction.
    Introduction,
    /// Provided value-adding annotations.
    Annotator,
    /// Commented the work.
    Commentator,
    /// Holds a patent or similar.
    Holder,
    /// Compiled the works in an [Anthology](hayagriva_core::EntryType::Anthology).
    Compiler,
    /// Founded the publication.
    Founder,
    /// Collaborated on the cited item.
    Collaborator,
    /// Organized the creation of the cited item.
    Organizer,
    /// Performed in the cited item.
    CastMember,
    /// Composed all or parts of the cited item's musical / audible components.
    Composer,
    /// Produced the cited item.
    Producer,
    /// Lead Producer for the cited item.
    ExecutiveProducer,
    /// Did the writing for the cited item.
    Writer,
    /// Shot film/video for the cited item.
    Cinematography,
    /// Directed the cited item.
    Director,
    /// Illustrated the cited item.
    Illustrator,
    /// Provided narration or voice-over for the cited item.
    Narrator,

    /// Various other roles described by the contained string.
    #[serde(skip)]
    Unknown(String),
}
