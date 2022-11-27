use super::parse_error::ParseErr;
use crate::ast::program::Program;
use crate::ast::typed_expression::TypedExpr;
use crate::lexer::{lex::Lexer, token::Token};
use crate::{
    ast::{
        expression::Expr, infix::Infix, precedence::Precedence, prefix::Prefix, statement::Stmt,
        ty::Type,
    },
    lexer::token::TokenInfo,
};

type PrefixParseFn = fn(&mut Parser) -> Result<TypedExpr, ParseErr>;
type InfixParseFn = fn(&mut Parser, TypedExpr) -> Result<TypedExpr, ParseErr>;

pub struct Parser {
    lexer: Lexer,
    current_token: TokenInfo,
    peek_token: TokenInfo,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: TokenInfo::new(Token::Illegal, 0, 0),
            peek_token: TokenInfo::new(Token::Illegal, 0, 0),
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
        self.peek_token = self.lexer.next_token();
    }

    fn expect_and_move(
        &mut self,
        token: Token,
        expected: fn(TokenInfo) -> ParseErr,
    ) -> Result<(), ParseErr> {
        if self.peek_token.token != token {
            return Err(expected(self.peek_token.clone()));
        }
        self.next_token();
        Ok(())
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseErr> {
        let mut statements = vec![];
        while self.current_token.token != Token::Eof {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token();
        }
        Ok(Program {
            sequence: Stmt::Sequence {
                statements: Box::new(statements),
            },
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseErr> {
        match self.current_token.token {
            Token::I64 | Token::Bool => self.parse_declaration_stmt(),
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
        let alternative = if self.peek_token.token == Token::Else {
            self.next_token();
            self.expect_and_move(Token::LBrace, ParseErr::ExpectedLbrace)?;
            Some(Box::new(self.parse_sequence()?))
        } else {
            None
        };
        self.expect_and_move(Token::Semicolon, ParseErr::ExpectedSemicolon)?;

        Ok(Stmt::If {
            condition: Box::new(condition),
            body: Box::new(consequence),
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
            body: Box::new(consequence),
        })
    }

    fn get_prefix_fn(&self) -> Option<PrefixParseFn> {
        match &self.current_token.token {
            Token::Identifier(_) => Some(Parser::parse_identifier_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            Token::LParenthesis => Some(Parser::parse_grouped_expression),
            Token::Number(_) => Some(Parser::parse_literal_expression),
            Token::True | Token::False => Some(Parser::parse_boolean_expression),
            _ => None,
        }
    }

    fn parse_sequence(&mut self) -> Result<Stmt, ParseErr> {
        let mut statements = vec![];

        self.next_token();
        while self.current_token.token != Token::RBrace && self.current_token.token != Token::Eof {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Stmt::Sequence {
            statements: Box::new(statements),
        })
    }

    fn parse_literal_expression(&mut self) -> Result<TypedExpr, ParseErr> {
        if let Token::Number(lit) = &self.current_token.token {
            match lit.parse::<i64>() {
                Ok(value) => Ok(TypedExpr {
                    ty: Some(Type::I64),
                    expr: Expr::IntegerLiteral(value),
                }),
                Err(error) => Err(ParseErr::ParseIntError(self.current_token.clone(), error)),
            }
        } else {
            Err(ParseErr::ExpectedLiteral(self.current_token.clone()))
        }
    }

    fn parse_boolean_expression(&mut self) -> Result<TypedExpr, ParseErr> {
        let value = match &self.current_token.token {
            Token::True => Some(Expr::BooleanLiteral(true)),
            Token::False => Some(Expr::BooleanLiteral(false)),
            _ => None,
        };
        match value {
            Some(expr) => Ok(TypedExpr {
                ty: Some(Type::Bool),
                expr: expr,
            }),
            None => Err(ParseErr::ExpectedBoolToken(self.current_token.clone())),
        }
    }

    fn get_infix_fn(&mut self) -> Option<InfixParseFn> {
        match &self.peek_token.token {
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

    fn get_prefix_token(&self, token: &TokenInfo) -> Result<Prefix, ParseErr> {
        match token.token {
            Token::Minus => Ok(Prefix::Minus),
            _ => Err(ParseErr::ExpectedPrefixToken(token.clone())),
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
        let prefix = self
            .get_prefix_fn()
            .ok_or_else(|| ParseErr::ExpectedPrefixToken(self.current_token.clone()))?;
        let mut left_expr = prefix(self)?;

        while self.peek_token.token != Token::Semicolon
            && precedence < self.get_infix_token(&self.peek_token.token).0
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
                ))
            }
        }?;
        let operator = match self.current_token.token {
            Token::Assign => Infix::Assign,
            Token::PlusEquals => Infix::PlusEquals,
            Token::MinusEquals => Infix::MinusEquals,
            Token::AsteriskEquals => Infix::AsteriskEquals,
            Token::SlashEuqals => Infix::SlashEuqals,
            _ => return Err(ParseErr::UnsupportedInfixToken(self.current_token.clone())),
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
        if let Some(ty) = Type::from_token(&self.current_token.token) {
            Ok(ty)
        } else {
            Err(ParseErr::TokenNotFound(self.current_token.clone()))
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
        if self.peek_token.token == Token::Semicolon {
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
        if let Token::Identifier(ident) = &self.current_token.token {
            Ok(ident.to_string())
        } else {
            Err(ParseErr::ExpectedIdentifierToken(
                self.current_token.clone(),
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
        let (precedence, infix) = self.get_infix_token(&self.current_token.token);
        let i = infix.ok_or_else(|| ParseErr::ExpectedInfixToken(self.current_token.clone()))?;
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
