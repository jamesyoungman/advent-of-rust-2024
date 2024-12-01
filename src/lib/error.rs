use std::fmt::{self, Display, Formatter};

/// Generic error type for when a typed error isn't useful.
#[derive(Debug, PartialEq, Eq)]
pub struct Fail(pub String);

impl Display for Fail {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.0.as_str())
    }
}

impl std::error::Error for Fail {}

pub fn fail_from_error(e: &dyn std::error::Error) -> Fail {
    Fail(e.to_string())
}
