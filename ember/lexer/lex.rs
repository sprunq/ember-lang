use crate::syntax::token::{lookup_ident, Token, TokenInfo};
use std::str::Chars;

pub struct Lexer<'source> {
    pub input: &'source str,
    pub iter: Chars<'source>,
    pub file_id: usize,
    position: usize,
    character: char,
}

impl<'source> Iterator for Lexer<'source> {
    type Item = TokenInfo;
    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok.token == Token::Eof {
            None
        } else {
            Some(tok)
        }
    }
}

impl<'source> Lexer<'source> {
    pub fn new(input: &'source str) -> Self {
        let mut iter = input.chars();
        Self {
            character: iter.next().unwrap_or('\0'),
            position: 0,
            file_id: 0,
            input,
            iter,
        }
    }

    pub fn new_file(input: &'source str, file_id: usize) -> Self {
        let mut lex = Self::new(input);
        lex.file_id = file_id;
        lex
    }

    pub fn next_token(&mut self) -> TokenInfo {
        self.skip_whitespace();
        let tok: Token;
        let start_pos = self.position;
        match &self.character {
            '<' => tok = Token::Lt,
            '>' => tok = Token::Gt,
            ';' => tok = Token::Semicolon,
            ':' => tok = Token::Colon,
            ',' => tok = Token::Comma,
            '(' => tok = Token::LParenthesis,
            ')' => tok = Token::RParenthesis,
            '{' => tok = Token::LBrace,
            '}' => tok = Token::RBrace,
            '\0' => tok = Token::Eof,
            '[' => {
                tok = Token::LBracket;
            }
            ']' => {
                tok = Token::RBracket;
            }
            '=' => {
                tok = {
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                };
            }
            '!' => {
                tok = {
                    if self.peek_char() == '=' {
                        self.read_char();
                        Token::NotEqual
                    } else {
                        Token::Illegal
                    }
                };
            }
            '+' => {
                tok = if self.peek_char() == '=' {
                    self.read_char();
                    Token::PlusEquals
                } else {
                    Token::Plus
                }
            }
            '-' => {
                tok = if self.peek_char() == '=' {
                    self.read_char();
                    Token::MinusEquals
                } else if self.peek_char() == '>' {
                    self.read_char();
                    Token::Arrow
                } else {
                    Token::Minus
                }
            }
            '/' => {
                tok = if self.peek_char() == '=' {
                    self.read_char();
                    Token::SlashEuqals
                } else if self.peek_char() == '/' {
                    while self.peek_char() != '\n' {
                        self.read_char();
                    }
                    return self.next_token();
                } else {
                    Token::Slash
                }
            }
            '*' => {
                tok = if self.peek_char() == '=' {
                    self.read_char();
                    Token::AsteriskEquals
                } else {
                    Token::Asterisk
                }
            }
            _ => {
                if Self::is_letter(self.character) && self.character != '_' {
                    let ident = self.read_identifier();
                    return TokenInfo::new(lookup_ident(ident), start_pos..self.position, 0);
                } else if Self::is_digit(self.character) {
                    self.consume_number();
                    return TokenInfo::new(Token::Number, start_pos..self.position, 0);
                } else {
                    tok = Token::Illegal
                }
            }
        };
        self.read_char();
        TokenInfo::new(tok, start_pos..self.position, 0)
    }

    fn skip_whitespace(&mut self) {
        while self.character.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn is_digit(character: char) -> bool {
        character.is_ascii_digit()
    }

    fn is_letter(character: char) -> bool {
        character.is_alphabetic() || character == '_'
    }

    pub fn read_identifier(&mut self) -> &'source str {
        let start_pos = self.position;
        while Self::is_letter(self.character) || Self::is_digit(self.character) {
            self.read_char();
        }
        let end_pos = self.position;
        &self.input[start_pos..end_pos]
    }

    fn consume_number(&mut self) {
        while Self::is_digit(self.character) || self.character == '.' {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        self.character = self.iter.next().unwrap_or('\0');
        self.position += 1;
    }

    fn peek_char(&mut self) -> char {
        self.iter.clone().next().unwrap_or('\0')
    }
}
