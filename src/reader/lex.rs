use std::{iter::Peekable, str::Chars};

#[allow(dead_code)]
#[derive(Debug, PartialEq, PartialOrd)]
pub enum TokenType {
    /// Literals
    /// #t | #f
    BoolLit(bool),

    // TODO: Split this into a Number subtype then add radix and rational number support
    IntLit(i64),
    RealLit(f64),

    CharLit(char),
    StrLit(String),

    /// (
    OpenParen,
    /// )
    CloseParen,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// #(
    OpenVector,
    /// ,
    Comma,
    /// ,@
    CommaAt,
    /// .
    Dot,
    /// `
    Backtick,

    // We say "fuck it. anything else is an identifier"
    Ident(String),

    /// ;.*\n
    Comment,
    Eof,
    // TODO: Maybe have these be lexed separately
    // Syntactic Keywords
    //Else,
    //Arrow,
    //Define,
    //Unquote,
    //UnquoteSplicing,

    //// Expression Keywords
    //Quote,
    //Lambda,
    //If,
    //Set,
    //Begin,
    //Cond,
    //And,
    //Or,
    //Case,
    //Let,
    //LetStar,
    //LetRec,
    //Do,
    //Delay,
    //Quasiquote,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
pub struct Span {
    pub begin: (usize, usize),
    pub end: (usize, usize),
}

impl Span {
    pub fn incr_col(old: Span) -> Span {
        Span {
            end: (old.end.0, old.end.1 + 1),
            ..old
        }
    }

    pub fn incr_line(old: Span) -> Span {
        Span {
            end: (old.end.0 + 1, 1),
            ..old
        }
    }
}

#[allow(dead_code)]
pub struct Token {
    pub kind: TokenType,
    pub span: Span,
}

#[allow(dead_code)]
impl Token {
    pub fn new(kind: TokenType, span: Span) -> Self {
        Self { kind, span }
    }
}

pub struct Lexer<'src> {
    input: &'src str,
    stream: Peekable<Chars<'src>>,
    pos: (usize, usize),
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            input,
            stream: input.chars().peekable(),
            pos: (1, 1),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token_span = Span {
            begin: self.pos,
            end: self.pos,
        };

        Token::new(TokenType::Eof, token_span)
    }

    fn skip_whitespace(&mut self) {
        while self.stream.next_if(|ch| ch.is_ascii_whitespace()).is_some() {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn whitespace_test() {}
}
