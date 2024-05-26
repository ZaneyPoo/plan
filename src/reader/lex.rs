// Info on Scheme's lexical structure from here:
// https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_9.html#SEC73
use super::span::{Position, Span};
use super::token::{Token, TokenType};
use crate::{etok, mk_tok, span, tok};
use std::{iter::Peekable, str::Chars};

pub struct Lexer<'src> {
    input: &'src str,
    stream: Peekable<Chars<'src>>,
    pos: Position,
    current: Option<char>,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            input,
            stream: input.chars().peekable(),
            pos: Position { line: 1, col: 1 },
            current: None,
        }
    }

    pub fn next_token(&mut self) -> Token {
        // TODO: Refactor this to be more multilayered 
        // since there's so many intersecting states
        self.skip_whitespace();
        let token_start = self.pos;

        let kind = match self.next_char() {
            Some('(') => tok![lparen],
            Some(')') => tok![rparen],
            Some('[') => tok![lbrack],
            Some(']') => tok![rbrack],
            Some('{') => tok![lcurly],
            Some('}') => tok![rcurly],
            Some(',') => match self.peek_char() {
                Some('@') => tok![,@],
                Some(_) | None => tok![,],
            },
            Some('\'') => tok![quote],
            Some(';') => self.consume_comment(),
            Some('"') => self.consume_string(),
            Some('#') => match self.next_char() {
                Some('t' | 'T') => tok![#t],
                Some('f' | 'F') => tok![#f],
                Some('(') => tok![vec],
                Some('\\') => self.consume_char_lit(),
                // Radix specifier
                Some('b' | 'B') => todo!(),
                Some('o' | 'O') => todo!(),
                Some('d' | 'D') => todo!(),
                Some('x' | 'X') => todo!(),
                // Exactness specifier
                Some('i' | 'I') => todo!(),
                Some('e' | 'E') => todo!(),

                Some(ch) => etok![expected "tf(\\bodx", got ch],
                None => etok![eof],
            },
            Some('.') => match self.peek_char() {
                Some(ch) if ch.is_ascii_digit() => {
                    let mut buf = ch.to_string();
                    self.consume_real(&mut buf)
                }
                Some(ch) => etok![invalid_ident format!(".{ch}")],
                None => etok![invalid_ident '.'],
            },
            Some('+') => match self.peek_char() {
                Some(ch) if ch.is_ascii_digit() => self.consume_number('+'),
                Some(ch) if is_delimiter(*ch) => tok![ident '+'],
                Some(ch) => etok![invalid_ident format!("+{ch}")],
                None => tok![ident '+'],
            },
            Some('-') => match self.peek_char() {
                Some(ch) if ch.is_ascii_digit() => self.consume_number('-'),
                Some(ch) if is_delimiter(*ch) => tok![ident '-'],
                Some(ch) => etok![invalid_ident format!("-{ch}")],
                None => tok![ident '-'],
            },
            Some('1'..='9') => self.consume_number('+'),
            Some(ch) if is_symbol_start(ch) => self.consume_ident(ch),
            Some(ch) => etok![unknown_char ch],

            None => etok![eof],
        };

        Token::new(kind, span![(token_start) to (self.pos)])
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_some_and(|c| c.is_ascii_whitespace()) {
            self.next_char();
        }
    }

    fn consume_comment(&mut self) -> TokenType {
        self.advance_while(|c| c != '\n', |_| {});
        tok![;]
    }

    fn consume_string(&mut self) -> TokenType {
        let mut buf = String::new();

        let last = self.fill_buf_while(|ch| ch != '"', &mut buf);

        match last {
            Some('"') => {
                tok![str buf]
            }
            Some(_) | None => {
                etok![unterm_str]
            }
        }
    }

    fn consume_char_lit(&mut self) -> TokenType {
        // TODO: Handle all the wacky named char literals
        match self.next_char() {
            Some(ch) => {
                self.advance_while(|c| !is_delimiter(c), |_| {});
                tok![char ch]
            }
            None => etok![unexpected_eof],
        }
    }

    fn consume_number(&mut self, sign: char) -> TokenType {
        // TODO: Handle exponent, exactness, and radix markers :puke:
        let mut buf = sign.to_string();

        let last = self.fill_buf_while(|c| c.is_ascii_digit() && c != '.', &mut buf);

        match last {
            Some('.') => self.consume_real(&mut buf),
            Some(ch) if is_delimiter(ch) => match buf.parse::<i64>() {
                Ok(num) => tok![int num],
                Err(e) => {
                    dbg!(buf, e.clone());
                    etok![invalid_int e]
                }
            },
            Some(ch) => etok![expected ".", got ch],
            None => etok![unexpected_eof],
        }
    }

    fn consume_real(&mut self, buf: &mut String) -> TokenType {
        let last = self.fill_buf_while(|c| c.is_ascii_digit(), buf);

        match last {
            Some(ch) if is_delimiter(ch) => match buf.parse::<f64>() {
                Ok(num) => tok![real num],
                Err(e) => etok![invalid_real e],
            },
            Some(ch) => etok![unexpected_char ch],
            None => etok![unexpected_eof],
        }
    }

    fn consume_ident(&mut self, first_char: char) -> TokenType {
        let mut buf = first_char.to_string();
        while self.peek_char().is_some_and(|c| !is_delimiter(*c)) {
            buf.push(self.next_char().expect("Char cannot be None"));
        }
        tok![ident buf]
    }

    fn next_char(&mut self) -> Option<char> {
        self.current = self.stream.next();

        if let Some(c) = self.current {
            if c == '\n' {
                self.pos.incr_line();
            } else {
                self.pos.incr_col();
            }
        };

        self.current
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.stream.peek()
    }

    fn advance(&mut self, distance: usize) -> Option<char> {
        for _ in 0..distance {
            self.next_char();
        }
        self.current
    }

    fn advance_while<Predicate, SideEffect>(
        &mut self,
        f: Predicate,
        mut g: SideEffect,
    ) -> Option<char>
    where
        Predicate: Fn(char) -> bool,
        SideEffect: FnMut(&mut Lexer),
    {
        while self.next_char().is_some_and(&f) {
            g(self);
        }

        self.current
    }

    fn fill_buf_while<Predicate>(&mut self, f: Predicate, buf: &mut String) -> Option<char>
    where
        Predicate: Fn(char) -> bool,
    {
        self.advance_while(f, |lexer| {
            if let Some(ch) = lexer.current {
                buf.push(ch)
            }
        })
    }
}

/// Scheme defines delimiters as:
/// ( ) [ ] { } ; " ' ` |
/// or any whitespace character
pub fn is_delimiter(ch: char) -> bool {
    ch.is_ascii_whitespace()
        || matches!(
            ch,
            '(' | ')' | ';' | '"' | '\'' | '`' | '|' | '[' | ']' | '{' | '}'
        )
}

pub fn is_symbol_start(ch: char) -> bool {
    ch.is_alphabetic()
        || matches!(
            ch,
            '+' | '-'
                | '.'
                | '*'
                | '/'
                | '<'
                | '='
                | '>'
                | '!'
                | '?'
                | ':'
                | '$'
                | '%'
                | '_'
                | '&'
                | '~'
                | '^'
        )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_tokens(sample: &str, answers: &[Token]) {
        let mut lexer = Lexer::new(sample);

        for answer in answers {
            let token = lexer.next_token();
            // TODO: Actually worry about the spans being correct
            dbg!(token.clone());
            assert_eq!(
                answer.kind, token.kind,
                "\nExpected: {answer}\nGot: {token}"
            )
        }
    }

    // These tests are shamelessly stolen from here: https://github.com/MattX/peroxide/blob/master/src/lex.rs
    #[test]
    fn char_test() {
        let sample = "#\\! \n#\\n \n#\\  n#\\newline \n#\\space \n#\\defS \n#\\";
        let answers = &[
            Token::new(tok![char '!'], span!(1,1 to 1,3)),
            Token::new(tok![char 'n'], span!(2,1 to 2,3)),
            Token::new(tok![char ' '], span!(3,1 to 3,3)),
            Token::new(tok![char '\n'], span!(4,1 to 4,9)),
            Token::new(tok![char ' '], span!(5,1 to 5,7)),
            Token::new(etok![expected "", got 'e'], span!(6,1 to 6,4)),
            Token::new(etok![unexpected_eof], span!(7,1 to 7,3)),
        ];

        assert_tokens(sample, answers)
    }

    #[test]
    fn int_test() {
        let sample = "123 \n0 \n-123 \n+123 \n#xfe \n12x3 \n123x";
        let answers = &[
            Token::new(tok![int 123], span![1,1 to 1,3]),
            Token::new(tok![int 0], span![2, 1]),
            Token::new(tok![int - 123], span![3,1 to 3,4]),
            Token::new(tok![int 123], span![4,1 to 4,4]),
            Token::new(tok![int 254], span![5,1 to 5,4]),
            Token::new(etok![expected ".", got 'x'], span![6,1 to 6,4]),
            Token::new(etok![expected ".", got 'x'], span![7,1 to 7,4]),
            Token::new(etok![eof], span![7, 5]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn real_test() {
        // jfc this one is gonna be impossible
        let sample = concat! {
            "123.4567\n",
            ".4567\n",
            "0.\n",
            "-0.\n",
            "0.06\n",
            "0.06d0\n",
            "0.06d2\n",
            "0.06d-2\n",
            "123#.##\n",
            "-inf.0\n",
            "+inf.0\n",
            "-0a.\n",
            "-0.123d\n",
            "-0.06e\n",
        };
        let answers = &[
            Token::new(tok![real 123.4567], span![1,1 to 1,8]),
            Token::new(tok![real 0.4567], span![2,1 to 2,5]),
            Token::new(tok![real 0.], span![3,1 to 3,2]),
            Token::new(tok![real - 0.], span![4,1 to 4,3]),
            Token::new(tok![real 0.06], span![5,1 to 5,4]),
            Token::new(tok![real 0.06e0], span![6,1 to 6,6]),
            Token::new(tok![real 0.06e2], span![7,1 to 7,6]),
            Token::new(tok![real 0.06e-2], span![8,1 to 8,7]),
            Token::new(tok![real 1230.00], span![9,1 to 9,7]),
            Token::new(tok![real f64::INFINITY], span![10,1 to 10,6]),
            Token::new(tok![real f64::NEG_INFINITY], span![11,1 to 11,6]),
            Token::new(etok![unexpected_char 'a'], span![12,1 to 12,3]),
            Token::new(etok![expected "1234567890-+", got 'd'], span![13,1 to 13,7]),
            Token::new(etok![expected "1234567890d", got 'e'], span![14,1 to 14,6]),
            Token::new(etok![eof], span![14, 8]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn bool_test() {
        let sample = "#f #t";
        let answers = &[
            Token::new(tok![#f], span![1,1 to 1,2]),
            Token::new(tok![#t], span![1,4 to 1,5]),
            Token::new(etok![eof], span![1, 6]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn parens_test() {
        let sample = "() \n(  ) ";
        let answers = &[
            Token::new(tok![lparen], span![1, 1]),
            Token::new(tok![rparen], span![1, 2]),
            Token::new(tok![lparen], span![2, 1]),
            Token::new(tok![rparen], span![2, 4]),
            Token::new(etok![eof], span![2, 6]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn ident_test() {
        let sample = concat! {
            "abc\n",
            "<=\n",
            "+\n",
            ".\n",
            "...\n",
        };
        let answers = &[
            Token::new(tok![ident "abc"], span![1,1 to 1,3]),
            Token::new(tok![ident "<="], span![2,1 to 2,2]),
            Token::new(tok![ident "+"], span![3,1 to 3,1]),
            Token::new(tok![ident "."], span![4,1 to 4,1]),
            Token::new(tok![ident "..."], span![5,1 to 5,3]),
            Token::new(etok![eof], span![6, 1]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn error_test() {
        // TODO: Test more of the error tokens
        let sample = concat! {
            "#\n",
            "\"abc\n",
        };
        let answers = &[
            Token::new(etok![expected "tf(\\bodx", got '\n'], span![1,1 to 1,2]),
            Token::new(etok![unterm_str], span![1,1 to 1,5]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn string_test() {
        let sample = concat! {
            "\"abcdef\"\n",
            "\"abc\\\"def\"\n",
            "\"abc\\\\def\"\n",
            "\"abc\\ndef\"\n",
        };
        let answers = &[
            Token::new(tok![str "abcdef"], span![1,2 to 1,7]),
            Token::new(tok![str "abc\"def"], span![2,2 to 2,9]),
            Token::new(tok![str "abc\\def"], span![3,2 to 3,9]),
            Token::new(tok![str "abc\ndef"], span![4,2 to 4,9]),
            Token::new(etok![eof], span![5, 1]),
        ];

        assert_tokens(sample, answers);
    }

    #[test]
    fn comment_test() {
        let sample = "; this is a comment\n(+ 2 ; this is a comment inline\n3)";
        let answers = &[
            Token::new(tok![;], span![1,1 to 1,20]),
            Token::new(tok![lparen], span!(2, 1)),
            Token::new(tok![ident "+"], span!(2, 2)),
            Token::new(tok![int 2], span!(2, 4)),
            Token::new(tok![;], span![2,6 to 2,32]),
            Token::new(tok![int 3], span!(3, 1)),
            Token::new(tok![rparen], span!(3, 2)),
            Token::new(etok![eof], span!(3, 3)),
        ];

        assert_tokens(sample, answers);
    }
}
