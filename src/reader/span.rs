use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

impl Position {
    pub fn incr_line(&mut self) {
        self.line += 1;
        self.col = 1;
    }

    pub fn incr_col(&mut self) {
        self.col += 1;
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Span {
    pub begin: Position,
    pub end: Position,
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.begin == self.end {
            write!(f, "{}", self.begin)
        } else {
            write!(f, "{} to {}", self.begin, self.end)
        }
    }
}

#[macro_export]
macro_rules! span {
    ($l:literal, $c:literal) => {
        Span {
            begin: Position { line: $l, col: $c },
            end: Position { line: $l, col: $c },
        }
    };
    [$bl:literal, $bc:literal to $el:literal, $ec:literal] => {
        Span {
            begin: Position { line: $bl, col: $bc },
            end: Position { line: $el, col: $ec },
        }
    };
    [($begin:expr) to ($end:expr)] => {
        Span {
            begin: $begin,
            end: $end,
        }
    };
}

