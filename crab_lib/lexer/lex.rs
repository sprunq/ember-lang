use super::token::{self, Token, TokenInfo};
use std::str::Chars;

pub struct Lexer<'source> {
    pub input: &'source str,
    pub iter: Chars<'source>, // uh oh. leaked string...
    position: usize,          // current position in input (points to current char)
    read_position: usize,     // current reading position in input (after current char)
    character: char,          // current char under examination
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
        let mut lexer = Lexer {
            character: '\u{0}',
            read_position: 0,
            position: 0,
            input,
            iter: input.chars(),
        };
        lexer.read_char();
        lexer
    }

    pub fn tokenize_all_collect(data: &'source str) -> Vec<TokenInfo> {
        let lex = Lexer::new(&data);
        lex.collect()
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
        self.character = if let Some(ch) = self.iter.next() {
            ch
        } else {
            '\0'
        };
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if let Some(ch) = self.iter.clone().next() {
            ch
        } else {
            '\0'
        }
    }
}
