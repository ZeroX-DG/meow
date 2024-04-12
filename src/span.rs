#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl From<((usize, usize), (usize, usize))> for Span {
    fn from(value: ((usize, usize), (usize, usize))) -> Self {
        Self {
            start: LineColumn {
                line: value.0 .0,
                column: value.0 .1,
            },
            end: LineColumn {
                line: value.1 .0,
                column: value.1 .1,
            },
        }
    }
}
