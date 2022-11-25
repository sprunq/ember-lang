use super::parse_error::ParseErr;
use crate::ast::{
    expression::{Expr, TypedExpr},
    infix::Infix,
    precedence::Precedence,
    prefix::Prefix,
    sequence::Sequence,
    statement::{Program, Stmt},
    ty::Type,
};
use crate::lexer::{
    lex::{Lexer, Position},
    token::{Literal, Token},
};

type PrefixParseFn = fn(&mut Parser) -> Result<TypedExpr, ParseErr>;
type InfixParseFn = fn(&mut Parser, TypedExpr) -> Result<TypedExpr, ParseErr>;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    peek_token_pos: Position,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::Illegal,
            peek_token: Token::Illegal,
            peek_token_pos: Position { line: 0, column: 0 },
        };
        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn input(&self) -> &str {
        &self.lexer.input
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        (self.peek_token, self.peek_token_pos) = self.lexer.next_token();
    }

    fn expect_and_move(
        &mut self,
        token: Token,
        expected: fn(Token, Position) -> ParseErr,
    ) -> Result<(), ParseErr> {
        if self.peek_token != token {
            return Err(expected(self.peek_token.clone(), self.peek_token_pos));
        }
        self.next_token();
        Ok(())
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseErr> {
        let mut statements = vec![];
        while self.current_token != Token::Eof {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token();
        }
        Ok(Program {
            sequence: statements,
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseErr> {
        match self.current_token {
            Token::I64 => self.parse_declaration_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::If => self.parse_if_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.expect_and_move(Token::LParenthesis, ParseErr::ExpectedLparen)?;
        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::RParenthesis, ParseErr::ExpectedRparen)?;
        self.expect_and_move(Token::LBrace, ParseErr::ExpectedLbrace)?;
        let consequence = self.parse_sequence()?;
        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            self.expect_and_move(Token::LBrace, ParseErr::ExpectedLbrace)?;
            Some(self.parse_sequence()?)
        } else {
            None
        };
        self.expect_and_move(Token::Semicolon, ParseErr::ExpectedSemicolon)?;

        Ok(Stmt::If {
            condition: Box::new(condition),
            body: consequence,
            alternative,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.expect_and_move(Token::LParenthesis, ParseErr::ExpectedLparen)?;
        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::RParenthesis, ParseErr::ExpectedRparen)?;
        self.expect_and_move(Token::LBrace, ParseErr::ExpectedLbrace)?;
        let consequence = self.parse_sequence()?;
        self.expect_and_move(Token::Semicolon, ParseErr::ExpectedSemicolon)?;

        Ok(Stmt::While {
            condition: Box::new(condition),
            body: consequence,
        })
    }

    fn get_prefix_fn(&self) -> Option<PrefixParseFn> {
        match &self.current_token {
            Token::Identifier(_) => Some(Parser::parse_identifier_expression),
            Token::Bang => Some(Parser::parse_prefix_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            Token::LParenthesis => Some(Parser::parse_grouped_expression),
            Token::Literal(_) => Some(Parser::parse_literal_expression),
            _ => None,
        }
    }

    fn parse_sequence(&mut self) -> Result<Sequence, ParseErr> {
        let mut statements = vec![];

        self.next_token();
        while self.current_token != Token::RBrace && self.current_token != Token::Eof {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Sequence { statements })
    }

    fn parse_literal_expression(&mut self) -> Result<TypedExpr, ParseErr> {
        if let Token::Literal(lit) = &self.current_token {
            match lit {
                Literal::Integer(value) => Ok(TypedExpr {
                    ty: Some(Type::I64),
                    expr: Expr::IntegerLiteral(*value),
                }),
            }
        } else {
            Err(ParseErr::CbaError())
        }
    }

    fn get_infix_fn(&mut self) -> Option<InfixParseFn> {
        match &self.peek_token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Equal
            | Token::NotEqual
            | Token::Lt
            | Token::Gt => Some(Parser::parse_infix_expression),
            Token::Assign
            | Token::PlusEquals
            | Token::MinusEquals
            | Token::SlashEuqals
            | Token::AsteriskEquals => Some(Parser::parse_assign_expression),
            _ => None,
        }
    }

    fn get_prefix_token(&self, token: &Token) -> Result<Prefix, ParseErr> {
        match token {
            Token::Bang => Ok(Prefix::Bang),
            Token::Minus => Ok(Prefix::Minus),
            token => Err(ParseErr::ExpectedPrefixToken(
                token.clone(),
                self.peek_token_pos,
            )),
        }
    }

    fn get_infix_token(&self, token: &Token) -> (Precedence, Option<Infix>) {
        match token {
            Token::Equal => (Precedence::Equals, Some(Infix::Eq)),
            Token::NotEqual => (Precedence::Equals, Some(Infix::NotEq)),
            Token::Lt => (Precedence::LessGreater, Some(Infix::Lt)),
            Token::Gt => (Precedence::LessGreater, Some(Infix::Gt)),
            Token::Plus => (Precedence::Sum, Some(Infix::Plus)),
            Token::Minus => (Precedence::Sum, Some(Infix::Minus)),
            Token::Slash => (Precedence::Product, Some(Infix::Slash)),
            Token::Asterisk => (Precedence::Product, Some(Infix::Asterisk)),
            Token::LParenthesis => (Precedence::Call, None),
            Token::LBracket => (Precedence::Index, None),
            Token::Assign => (Precedence::Assign, Some(Infix::Assign)),
            Token::PlusEquals => (Precedence::Assign, Some(Infix::PlusEquals)),
            Token::MinusEquals => (Precedence::Assign, Some(Infix::MinusEquals)),
            Token::SlashEuqals => (Precedence::Assign, Some(Infix::SlashEuqals)),
            Token::AsteriskEquals => (Precedence::Assign, Some(Infix::AsteriskEquals)),
            _ => (Precedence::Lowest, None),
        }
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<TypedExpr, ParseErr> {
        let prefix = self.get_prefix_fn().ok_or_else(|| {
            ParseErr::ExpectedPrefixToken(self.current_token.clone(), self.peek_token_pos)
        })?;
        let mut left_expr = prefix(self)?;

        while self.peek_token != Token::Semicolon
            && precedence < self.get_infix_token(&self.peek_token).0
        {
            if let Some(infix) = self.get_infix_fn() {
                self.next_token();
                left_expr = infix(self, left_expr)?;
            } else {
                return Ok(left_expr);
            }
        }
        Ok(left_expr)
    }

    fn parse_assign_expression(&mut self, left: TypedExpr) -> Result<TypedExpr, ParseErr> {
        let name = {
            if let Expr::Identifier(ident) = left.expr {
                Ok(ident)
            } else {
                Err(ParseErr::ExpectedIdentifierToken(
                    self.current_token.clone(),
                    self.peek_token_pos,
                ))
            }
        }?;
        let operator = match self.current_token {
            Token::Assign => Infix::Assign,
            Token::PlusEquals => Infix::PlusEquals,
            Token::MinusEquals => Infix::MinusEquals,
            Token::AsteriskEquals => Infix::AsteriskEquals,
            Token::SlashEuqals => Infix::SlashEuqals,
            _ => {
                return Err(ParseErr::UnsupportedInfixToken(
                    self.current_token.clone(),
                    self.peek_token_pos,
                ))
            }
        };
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        Ok(TypedExpr::new(Expr::Assign {
            ident: name,
            operand: operator,
            expr: Box::new(value),
        }))
    }

    fn parse_type(&mut self) -> Result<Type, ParseErr> {
        if let Some(ty) = Type::from_token(&self.current_token) {
            Ok(ty)
        } else {
            Err(ParseErr::TokenNotFound(
                self.current_token.clone(),
                self.peek_token_pos,
            ))
        }
    }

    fn parse_declaration_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let ty = self.parse_type()?;
        self.next_token();
        let ident = self.parse_identifier_string()?;
        self.expect_and_move(Token::Assign, ParseErr::ExpectedAssign)?;
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::Semicolon, ParseErr::ExpectedSemicolon)?;
        Ok(Stmt::Declaration { ty, ident, value })
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let expression = self.parse_expr(Precedence::Lowest)?;
        if self.peek_token == Token::Semicolon {
            self.next_token();
        }
        Ok(Stmt::Expression { expr: expression })
    }

    fn parse_identifier_expression(&mut self) -> Result<TypedExpr, ParseErr> {
        match self.parse_identifier_string().map(Expr::Identifier) {
            Ok(expr) => Ok(TypedExpr::new(expr)),
            Err(err) => Err(err),
        }
    }

    fn parse_identifier_string(&self) -> Result<String, ParseErr> {
        if let Token::Identifier(ident) = &self.current_token {
            Ok(ident.to_string())
        } else {
            Err(ParseErr::ExpectedIdentifierToken(
                self.current_token.clone(),
                self.peek_token_pos,
            ))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<TypedExpr, ParseErr> {
        let prefix_token = self.get_prefix_token(&self.current_token)?;
        self.next_token();
        let right_expr = self.parse_expr(Precedence::Prefix)?;
        Ok(TypedExpr::new(Expr::Prefix {
            op: prefix_token,
            expr: Box::new(right_expr),
        }))
    }

    fn parse_infix_expression(&mut self, left: TypedExpr) -> Result<TypedExpr, ParseErr> {
        let (precedence, infix) = self.get_infix_token(&self.current_token);
        let i = infix.ok_or_else(|| {
            ParseErr::ExpectedInfixToken(self.current_token.clone(), self.peek_token_pos)
        })?;
        self.next_token();
        let right = self.parse_expr(precedence)?;

        Ok(TypedExpr::new(Expr::Infix {
            op: i,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }

    fn parse_grouped_expression(&mut self) -> Result<TypedExpr, ParseErr> {
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::RParenthesis, ParseErr::ExpectedRparen)?;
        Ok(expr)
    }
}
