use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Literal(Literal),
    Identifier(String),
    Illegal,
    Eof,
    Comma,
    Semicolon,
    LParenthesis,
    RParenthesis,
    LBrace,
    RBrace,
    True,
    False,
    If,
    Else,
    Return,
    Assign,
    PlusEquals,
    MinusEquals,
    SlashEuqals,
    AsteriskEquals,
    Plus,
    Minus,
    Bang,
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
    I64,
}

pub fn lookup_ident(ident: &str) -> Token {
    let token = match ident {
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "return" => Some(Token::Return),
        "while" => Some(Token::While),
        "i64" => Some(Token::I64),
        _ => None,
    };
    if let Some(token) = token {
        token
    } else {
        Token::Identifier(ident.to_owned())
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
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Colon => write!(f, ":"),
            Token::While => write!(f, "for"),
            Token::PlusEquals => write!(f, "+="),
            Token::MinusEquals => write!(f, "-="),
            Token::SlashEuqals => write!(f, "/="),
            Token::AsteriskEquals => write!(f, "*="),
            Token::Identifier(ident) => write!(f, "{}", ident),
            Token::Literal(lit) => write!(f, "{}", lit),
            Token::I64 => write!(f, "i64"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(i64),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Literal::Integer(value) => write!(f, "{}", value),
        }
    }
}
