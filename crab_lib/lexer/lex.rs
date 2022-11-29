use std::str::Chars;

use super::token::{self, Token, TokenInfo};

pub struct Lexer {
    pub input: Chars<'static>,
    position: usize,      // current position in input (points to current char)
    read_position: usize, // current reading position in input (after current char)
    character: char,      // current char under examination
}

fn string_to_static_str(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input: string_to_static_str(input).chars(),
            character: '\u{0}',
            read_position: 0,
            position: 0,
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> TokenInfo {
        self.skip_whitespace();
        let tok: Token;
        let start_pos = self.position;
        match &self.character {
            '<' => tok = Token::Lt,
            '>' => tok = Token::Gt,
            ';' => tok = Token::Semicolon,
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
                    return TokenInfo::new(token::lookup_ident(&ident), start_pos, self.position);
                } else if Self::is_digit(self.character) {
                    self.read_number();
                    return TokenInfo::new(Token::Number, start_pos, self.position);
                } else {
                    tok = Token::Illegal
                }
            }
        };
        self.read_char();
        TokenInfo::new(tok, start_pos, self.position)
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

    pub fn read_identifier(&mut self) -> String {
        let mut ident = String::from("");
        while Self::is_letter(self.character) || Self::is_digit(self.character) {
            ident.push(self.character);
            self.read_char();
        }
        ident
    }

    fn read_number(&mut self) -> String {
        let mut ident = String::from("");
        while Self::is_digit(self.character) || self.character == '.' {
            ident.push(self.character);
            self.read_char();
        }
        ident
    }

    fn read_char(&mut self) {
        self.character = self.input.next().unwrap_or('\0');
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        *self.input.clone().peekable().peek().unwrap_or(&'\0')
    }
}
