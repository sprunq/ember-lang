use super::{
    parse_error::ParseErr,
    syntax::{Expr, Spanned, Stmt},
    syntax::{InfixOp, Precedence, PrefixOp},
    ty::Type,
};
use crate::lexer::token::{Token, TokenInfo};
use std::ops::Range;

type InfixFunctionType<'source> =
    Option<for<'r> fn(&'r mut Parser<'source>, Spanned<Expr>) -> Result<Spanned<Expr>, ParseErr>>;
type PrefixFunctionType<'source> =
    Option<for<'r> fn(&'r mut Parser<'source>) -> Result<Spanned<Expr>, ParseErr>>;
type TypedFunctionParameter = (Spanned<String>, Spanned<Type>);

pub struct Parser<'source> {
    pub tokens: Vec<TokenInfo>,
    source: &'source str,
    current_token: TokenInfo,
    peek_token: TokenInfo,
    token_idx: usize,
}

impl<'source> Parser<'source> {
    pub fn new(tokens: Vec<TokenInfo>, source: &'source str) -> Self {
        let mut parser = Parser {
            current_token: TokenInfo::new(Token::Illegal, 0..0, 0),
            peek_token: TokenInfo::new(Token::Illegal, 0..0, 0),
            source,
            tokens,
            token_idx: 0,
        };

        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = if let Some(token) = self.tokens.get(self.token_idx) {
            token.to_owned()
        } else {
            TokenInfo::new(Token::Eof, 0..0, 0)
        };
        self.token_idx += 1;
    }

    fn expect_and_move(&mut self, expected: Token) -> Result<(), ParseErr> {
        if self.peek_token.token != expected {
            return Err(ParseErr::TokenMismatch {
                expected,
                actual: self.peek_token.clone(),
            });
        }
        self.next_token();
        Ok(())
    }

    pub fn get_str(&self, span: Range<usize>) -> &'source str {
        &self.source[span]
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, Vec<ParseErr>> {
        let mut statements = vec![];
        while self.current_token.token != Token::Eof {
            match self.parse_top_level_statement() {
                Ok(a) => statements.push(a),
                Err(b) => return Err(vec![b]),
            }
            self.next_token();
        }
        Ok(statements)
    }

    fn parse_top_level_statement(&mut self) -> Result<Stmt, ParseErr> {
        match self.current_token.token {
            Token::Function => self.parse_define_function_stmt(),
            _ => Err(ParseErr::NotATopLevelStatement(self.current_token.clone())),
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseErr> {
        match self.current_token.token {
            Token::Let => self.parse_declaration_stmt(),
            Token::While => self.parse_while_stmt(),
            Token::If => self.parse_if_stmt(),
            Token::Function => self.parse_define_function_stmt(),
            Token::Return => self.parse_return_stmt(),
            Token::Break => self.parse_break_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.expect_and_move(Token::Semicolon)?;
        Ok(Stmt::Break)
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParseErr> {
        if self.peek_token.token == Token::Semicolon {
            self.expect_and_move(Token::Semicolon)?;
            Ok(Stmt::Return { value: None })
        } else {
            self.next_token();
            let expr = self.parse_expr(Precedence::Lowest)?;
            self.expect_and_move(Token::Semicolon)?;
            Ok(Stmt::Return { value: Some(expr) })
        }
    }

    fn parse_function_parameter(&mut self) -> Result<TypedFunctionParameter, ParseErr> {
        let start_pos = self.current_token.span.start;
        let ident = self.parse_identifier_string()?;
        self.expect_and_move(Token::Colon)?;
        self.next_token();
        let ty = self.parse_type()?;
        let end_type = self.current_token.span.end;
        let expr_sp = Spanned::new(ident.0, ident.1);
        let type_sp = Spanned::new(ty, start_pos..end_type);
        Ok((expr_sp, type_sp))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<TypedFunctionParameter>, ParseErr> {
        let mut identifiers = vec![];
        if self.peek_token.token == Token::RParenthesis {
            return Ok(identifiers);
        }
        self.next_token();
        identifiers.push(self.parse_function_parameter()?);

        while self.peek_token.token == Token::Comma {
            self.next_token();
            self.next_token();
            identifiers.push(self.parse_function_parameter()?);
        }
        Ok(identifiers)
    }

    fn parse_define_function_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.next_token();
        let name = self.parse_identifier_string()?;
        self.expect_and_move(Token::LParenthesis)?;
        let parameters = self.parse_function_parameters()?;
        self.expect_and_move(Token::RParenthesis)?;
        let ty = if self.peek_token.token == Token::Arrow {
            self.next_token();
            self.next_token();
            let ty_start = self.current_token.span.start;
            let ty_opt = self.parse_type()?;
            Some(Spanned::new(ty_opt, ty_start..self.current_token.span.end))
        } else {
            None
        };
        self.expect_and_move(Token::LBrace)?;

        let body = self.parse_sequence()?;
        if self.peek_token.token == Token::Semicolon {
            self.next_token();
        }
        Ok(Stmt::FunctionDefinition {
            name: Spanned::new(name.0, name.1),
            parameters,
            return_type: ty,
            body: Box::new(body),
        })
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::LBrace)?;
        let consequence = self.parse_sequence()?;
        let alternative = if self.peek_token.token == Token::Else {
            self.next_token();
            self.expect_and_move(Token::LBrace)?;
            Some(Box::new(self.parse_sequence()?))
        } else {
            None
        };
        if self.peek_token.token == Token::Semicolon {
            self.next_token();
        }

        Ok(Stmt::If {
            condition: Box::new(condition),
            body: Box::new(consequence),
            alternative,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.next_token();
        let condition = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::LBrace)?;
        let consequence = self.parse_sequence()?;
        if self.peek_token.token == Token::Semicolon {
            self.next_token();
        }

        Ok(Stmt::While {
            condition: Box::new(condition),
            body: Box::new(consequence),
        })
    }

    fn get_prefix_fn(&self) -> PrefixFunctionType<'source> {
        match &self.current_token.token {
            Token::Identifier => Some(Parser::parse_identifier_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            Token::LParenthesis => Some(Parser::parse_grouped_expression),
            Token::IntLiteral => Some(Parser::parse_int_literal_expression),
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

    fn parse_int_literal_expression(&mut self) -> Result<Spanned<Expr>, ParseErr> {
        if let Token::IntLiteral = &self.current_token.token {
            match &self.get_str(self.current_token.span.clone()).parse::<i64>() {
                Ok(value) => Ok(Spanned::new(
                    Expr::IntegerLiteral(Spanned::<i64>::new(
                        *value,
                        self.current_token.span.clone(),
                    )),
                    self.current_token.span.clone(),
                )),
                Err(error) => Err(ParseErr::ParseIntError(
                    self.current_token.clone(),
                    error.clone(),
                )),
            }
        } else {
            Err(ParseErr::TokenMismatch {
                expected: Token::IntLiteral,
                actual: self.current_token.clone(),
            })
        }
    }

    fn parse_boolean_expression(&mut self) -> Result<Spanned<Expr>, ParseErr> {
        let value = match &self.current_token.token {
            Token::True => Some(Expr::BooleanLiteral(Spanned::<bool>::new(
                true,
                self.current_token.span.clone(),
            ))),
            Token::False => Some(Expr::BooleanLiteral(Spanned::<bool>::new(
                false,
                self.current_token.span.clone(),
            ))),
            _ => None,
        };
        match value {
            Some(expr) => Ok(Spanned::new(expr, self.current_token.span.clone())),
            None => Err(ParseErr::TokenMismatch {
                expected: Token::Bool,
                actual: self.current_token.clone(),
            }),
        }
    }

    fn get_infix_fn(&mut self) -> InfixFunctionType<'source> {
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
            Token::LParenthesis => Some(Parser::parse_call_expression),
            _ => None,
        }
    }

    fn get_prefix_token(&self, token: &TokenInfo) -> Result<PrefixOp, ParseErr> {
        match token.token {
            Token::Minus => Ok(PrefixOp::Minus),
            _ => Err(ParseErr::ExpectedPrefixToken {
                actual: token.clone(),
            }),
        }
    }

    fn get_infix_token(&self, token: &Token) -> (Precedence, Option<InfixOp>) {
        match token {
            Token::Equal => (Precedence::Equals, Some(InfixOp::Eq)),
            Token::NotEqual => (Precedence::Equals, Some(InfixOp::NotEq)),
            Token::Lt => (Precedence::LessGreater, Some(InfixOp::Lt)),
            Token::Gt => (Precedence::LessGreater, Some(InfixOp::Gt)),
            Token::Plus => (Precedence::Sum, Some(InfixOp::Plus)),
            Token::Minus => (Precedence::Sum, Some(InfixOp::Minus)),
            Token::Slash => (Precedence::Product, Some(InfixOp::Slash)),
            Token::Asterisk => (Precedence::Product, Some(InfixOp::Asterisk)),
            Token::LParenthesis => (Precedence::Call, None),
            Token::LBracket => (Precedence::Index, None),
            Token::Assign => (Precedence::Assign, Some(InfixOp::Assign)),
            Token::PlusEquals => (Precedence::Assign, Some(InfixOp::PlusEquals)),
            Token::MinusEquals => (Precedence::Assign, Some(InfixOp::MinusEquals)),
            Token::SlashEuqals => (Precedence::Assign, Some(InfixOp::SlashEuqals)),
            Token::AsteriskEquals => (Precedence::Assign, Some(InfixOp::AsteriskEquals)),
            _ => (Precedence::Lowest, None),
        }
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Spanned<Expr>, ParseErr> {
        let prefix = self
            .get_prefix_fn()
            .ok_or_else(|| ParseErr::ExpectedPrefixToken {
                actual: self.current_token.clone(),
            })?;

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

    fn parse_call_expression(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseErr> {
        let start = left.pos.start;
        let args = self.parse_expressions(Token::RParenthesis)?;
        let inv = Expr::FunctionInvocation {
            name: Spanned::new(left.inner.to_string(), left.pos),
            args,
        };
        let end = self.current_token.span.end;
        Ok(Spanned::new(inv, start..end))
    }

    fn parse_expressions(&mut self, closing_token: Token) -> Result<Vec<Spanned<Expr>>, ParseErr> {
        let mut exps = vec![];
        if self.peek_token.token == closing_token {
            self.next_token();
            return Ok(exps);
        }
        self.next_token();
        exps.push(self.parse_expr(Precedence::Lowest)?);
        while self.peek_token.token == Token::Comma {
            self.next_token();
            self.next_token();
            exps.push(self.parse_expr(Precedence::Lowest)?);
        }
        self.expect_and_move(closing_token)?;

        Ok(exps)
    }

    fn parse_assign_expression(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseErr> {
        let operator = match self.current_token.token {
            Token::Assign => InfixOp::Assign,
            Token::PlusEquals => InfixOp::PlusEquals,
            Token::MinusEquals => InfixOp::MinusEquals,
            Token::AsteriskEquals => InfixOp::AsteriskEquals,
            Token::SlashEuqals => InfixOp::SlashEuqals,
            _ => return Err(ParseErr::UnsupportedInfixToken(self.current_token.clone())),
        };
        let operator_pos = self.current_token.span.clone();
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        let end_pos = value.pos.end;
        let ident_pos = left.pos.start;
        Ok(Spanned::new(
            Expr::Assign {
                ident: Spanned::new(left.inner.to_string(), left.pos),
                operand: Spanned::<InfixOp>::new(operator, operator_pos),
                expr: Box::new(value),
            },
            ident_pos..end_pos,
        ))
    }

    fn parse_type(&mut self) -> Result<Type, ParseErr> {
        if let Some(ty) = Type::from_token(&self.current_token.token) {
            Ok(ty)
        } else {
            Err(ParseErr::TokenNotFound(self.current_token.clone()))
        }
    }

    // let a [: int] = 20;
    fn parse_declaration_stmt(&mut self) -> Result<Stmt, ParseErr> {
        self.next_token();
        let ident = self.parse_identifier_string()?;
        // type declaration
        let ty = if self.peek_token.token == Token::Colon {
            self.next_token();
            self.next_token();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect_and_move(Token::Assign)?;
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::Semicolon)?;
        Ok(Stmt::Declaration {
            ty,
            ident: Spanned::new(ident.0, ident.1),
            value,
        })
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let expression = self.parse_expr(Precedence::Lowest)?;
        if self.peek_token.token == Token::Semicolon {
            self.next_token();
        }
        Ok(Stmt::Expression { expr: expression })
    }

    fn parse_identifier_expression(&mut self) -> Result<Spanned<Expr>, ParseErr> {
        let s = self.parse_identifier_string();
        match s {
            Ok(st) => Ok(Spanned::new(
                Expr::Identifier(Spanned::<String>::new(st.0, st.1.clone())),
                st.1,
            )),
            Err(err) => Err(err),
        }
    }

    fn parse_identifier_string(&self) -> Result<(String, Range<usize>), ParseErr> {
        if let Token::Identifier = &self.current_token.token {
            Ok((
                self.get_str(self.current_token.span.clone()).to_string(),
                self.current_token.span.clone(),
            ))
        } else {
            Err(ParseErr::TokenMismatch {
                expected: Token::Identifier,
                actual: self.current_token.clone(),
            })
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Spanned<Expr>, ParseErr> {
        let prefix_pos = self.current_token.span.clone();
        let prefix_token = self.get_prefix_token(&self.current_token)?;
        self.next_token();
        let right_expr = self.parse_expr(Precedence::Prefix)?;
        Ok(Spanned::new(
            Expr::Unary {
                op: Spanned::<PrefixOp>::new(prefix_token, prefix_pos.clone()),
                expr: Box::new(right_expr.clone()),
            },
            prefix_pos.start..right_expr.pos.end,
        ))
    }

    fn parse_infix_expression(&mut self, left: Spanned<Expr>) -> Result<Spanned<Expr>, ParseErr> {
        let (precedence, infix) = self.get_infix_token(&self.current_token.token);
        let i = infix.ok_or_else(|| ParseErr::ExpectedInfixToken {
            actual: self.current_token.clone(),
        })?;
        let infix_pos = self.current_token.span.clone();
        self.next_token();
        let right = self.parse_expr(precedence)?;

        Ok(Spanned::new(
            Expr::Binary {
                op: Spanned::<InfixOp>::new(i, infix_pos),
                left: Box::new(left.clone()),
                right: Box::new(right.clone()),
            },
            left.pos.start..right.pos.end,
        ))
    }

    fn parse_grouped_expression(&mut self) -> Result<Spanned<Expr>, ParseErr> {
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::RParenthesis)?;
        Ok(expr)
    }
}
