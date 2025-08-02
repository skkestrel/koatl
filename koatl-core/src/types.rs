#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Any,
    NoType,
    Unknown,
    Top,
    Bottom,
}
