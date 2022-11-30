use std::{fmt, ops::Range};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenInfo {
    pub token: Token,
    pub span: Range<usize>,
    pub file_id: usize,
}

impl TokenInfo {
    pub fn new(token: Token, span: Range<usize>, file_id: usize) -> Self {
        TokenInfo {
            token,
            span,
            file_id,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Number,
    Identifier,
    Illegal,
    Eof,
    Comma,
    Semicolon,
    LParenthesis,
    RParenthesis,
    LBrace,
    RBrace,
    If,
    Else,
    Assign,
    PlusEquals,
    MinusEquals,
    SlashEuqals,
    AsteriskEquals,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Equal,
    NotEqual,
    LBracket,
    RBracket,
    Colon,
    While,
    True,
    False,
    I64,
    Void,
    Bool,
    Function,
    Arrow,
}

pub fn lookup_ident(ident: &str) -> Token {
    let token = match ident {
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "while" => Some(Token::While),
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "i64" => Some(Token::I64),
        "bool" => Some(Token::Bool),
        "void" => Some(Token::Void),
        "fn" => Some(Token::Function),
        _ => None,
    };
    if let Some(token) = token {
        token
    } else {
        Token::Identifier
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Token::Illegal => write!(f, "ILLEGAL"),
            Token::Eof => write!(f, "EOF"),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LParenthesis => write!(f, "("),
            Token::RParenthesis => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Colon => write!(f, ":"),
            Token::While => write!(f, "while"),
            Token::PlusEquals => write!(f, "+="),
            Token::MinusEquals => write!(f, "-="),
            Token::SlashEuqals => write!(f, "/="),
            Token::AsteriskEquals => write!(f, "*="),
            Token::Identifier => write!(f, ""),
            Token::Number => write!(f, ""),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::I64 => write!(f, "i64"),
            Token::Bool => write!(f, "bool"),
            Token::Function => write!(f, "fn"),
            Token::Arrow => write!(f, "->"),
            Token::Void => write!(f, "void"),
        }
    }
}
