use super::parse_error::ParseErr;
use crate::ast::ast_node::AstNode;
use crate::ast::ast_root::AstRoot;
use crate::ast::typed_expression::TypedExpr;
use crate::lexer::{lex::Lexer, token::Token};
use crate::{
    ast::{
        expression::Expr, infix::InfixOp, precedence::Precedence, prefix::PrefixOp,
        statement::Stmt, ty::Type,
    },
    lexer::token::TokenInfo,
};
use std::ops::Range;

type PrefixParseFn = fn(&mut Parser) -> Result<AstNode<TypedExpr>, ParseErr>;
type InfixParseFn = fn(&mut Parser, AstNode<TypedExpr>) -> Result<AstNode<TypedExpr>, ParseErr>;

pub struct Parser {
    lexer: Lexer,
    current_token: TokenInfo,
    peek_token: TokenInfo,
    input: String,
}

impl Parser {
    pub fn new(input: String) -> Self {
        let input2 = input.clone();
        let mut parser = Parser {
            lexer: Lexer::new(input),
            current_token: TokenInfo::new(Token::Illegal, 0, 0),
            peek_token: TokenInfo::new(Token::Illegal, 0, 0),
            input: input2,
        };

        parser.next_token();
        parser.next_token();
        parser
    }

    pub fn next_token(&mut self) {
        self.current_token = self.peek_token.to_owned();
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

    pub fn parse_program(&mut self) -> Result<AstRoot, ParseErr> {
        let mut statements = vec![];
        while self.current_token.token != Token::Eof {
            let statement = self.parse_statement()?;
            statements.push(statement);
            self.next_token();
        }
        Ok(AstRoot {
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
            Token::Identifier => Some(Parser::parse_identifier_expression),
            Token::Minus => Some(Parser::parse_prefix_expression),
            Token::LParenthesis => Some(Parser::parse_grouped_expression),
            Token::Number => Some(Parser::parse_literal_expression),
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

    fn parse_literal_expression(&mut self) -> Result<AstNode<TypedExpr>, ParseErr> {
        if let Token::Number = &self.current_token.token {
            match self
                .current_token
                .clone()
                .get_str(&self.input)
                .parse::<i64>()
            {
                Ok(value) => Ok(AstNode::new(
                    TypedExpr {
                        ty: Some(Type::I64),
                        expr: Expr::IntegerLiteral(AstNode::<i64>::new(
                            value,
                            self.current_token.span.clone(),
                        )),
                    },
                    self.current_token.span.clone(),
                )),
                Err(error) => Err(ParseErr::ParseIntError(self.current_token.clone(), error)),
            }
        } else {
            Err(ParseErr::ExpectedLiteral(self.current_token.clone()))
        }
    }

    fn parse_boolean_expression(&mut self) -> Result<AstNode<TypedExpr>, ParseErr> {
        let value = match &self.current_token.token {
            Token::True => Some(Expr::BooleanLiteral(AstNode::<bool>::new(
                true,
                self.current_token.span.clone(),
            ))),
            Token::False => Some(Expr::BooleanLiteral(AstNode::<bool>::new(
                false,
                self.current_token.span.clone(),
            ))),
            _ => None,
        };
        match value {
            Some(expr) => Ok(AstNode::new(
                TypedExpr {
                    ty: Some(Type::Bool),
                    expr,
                },
                self.current_token.span.clone(),
            )),
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

    fn get_prefix_token(&self, token: &TokenInfo) -> Result<PrefixOp, ParseErr> {
        match token.token {
            Token::Minus => Ok(PrefixOp::Minus),
            _ => Err(ParseErr::ExpectedPrefixToken(token.clone())),
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

    fn parse_expr(&mut self, precedence: Precedence) -> Result<AstNode<TypedExpr>, ParseErr> {
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

    fn parse_assign_expression(
        &mut self,
        left: AstNode<TypedExpr>,
    ) -> Result<AstNode<TypedExpr>, ParseErr> {
        let identifier = {
            if let Expr::Identifier(ident) = left.inner.expr {
                Ok(ident)
            } else {
                Err(ParseErr::ExpectedIdentifierToken(
                    self.current_token.clone(),
                ))
            }
        }?;
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
        let end_pos = value.pos.end.clone();
        let pos = identifier.pos.clone();
        let ident = AstNode::new_boxed(TypedExpr::new(Expr::Identifier(identifier)), pos);
        let ident_pos = ident.pos.start;
        Ok(AstNode::new(
            TypedExpr::new(Expr::Assign {
                ident: ident,
                operand: AstNode::<InfixOp>::new(operator, operator_pos),
                expr: Box::new(value),
            }),
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

    fn parse_declaration_stmt(&mut self) -> Result<Stmt, ParseErr> {
        let ty = self.parse_type()?;
        self.next_token();
        let ident = self.parse_identifier_string()?;
        self.expect_and_move(Token::Assign, ParseErr::ExpectedAssign)?;
        self.next_token();
        let value = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::Semicolon, ParseErr::ExpectedSemicolon)?;
        Ok(Stmt::Declaration {
            ty,
            ident: AstNode::new(
                TypedExpr::new(Expr::Identifier(AstNode::<String>::new(
                    ident.0,
                    ident.1.clone(),
                ))),
                ident.1,
            ),
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

    fn parse_identifier_expression(&mut self) -> Result<AstNode<TypedExpr>, ParseErr> {
        let s = self.parse_identifier_string();
        match s {
            Ok(st) => Ok(AstNode::new(
                TypedExpr::new(Expr::Identifier(AstNode::<String>::new(st.0, st.1.clone()))),
                st.1,
            )),
            Err(err) => Err(err),
        }
    }

    fn parse_identifier_string(&self) -> Result<(String, Range<usize>), ParseErr> {
        if let Token::Identifier = &self.current_token.token {
            Ok((
                self.current_token.clone().get_str(&self.input).to_string(),
                self.current_token.span.clone(),
            ))
        } else {
            Err(ParseErr::ExpectedIdentifierToken(
                self.current_token.clone(),
            ))
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<AstNode<TypedExpr>, ParseErr> {
        let prefix_pos = self.current_token.span.clone();
        let prefix_token = self.get_prefix_token(&self.current_token)?;
        self.next_token();
        let right_expr = self.parse_expr(Precedence::Prefix)?;
        Ok(AstNode::new(
            TypedExpr::new(Expr::Prefix {
                op: AstNode::<PrefixOp>::new(prefix_token, prefix_pos.clone()),
                expr: Box::new(right_expr.clone()),
            }),
            prefix_pos.start..right_expr.pos.end,
        ))
    }

    fn parse_infix_expression(
        &mut self,
        left: AstNode<TypedExpr>,
    ) -> Result<AstNode<TypedExpr>, ParseErr> {
        let (precedence, infix) = self.get_infix_token(&self.current_token.token);
        let i = infix.ok_or_else(|| ParseErr::ExpectedInfixToken(self.current_token.clone()))?;
        let infix_pos = self.current_token.span.clone();
        self.next_token();
        let right = self.parse_expr(precedence)?;

        Ok(AstNode::new(
            TypedExpr::new(Expr::Infix {
                op: AstNode::<InfixOp>::new(i, infix_pos),
                left: Box::new(left.clone()),
                right: Box::new(right.clone()),
            }),
            left.pos.start..right.pos.end,
        ))
    }

    fn parse_grouped_expression(&mut self) -> Result<AstNode<TypedExpr>, ParseErr> {
        self.next_token();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_and_move(Token::RParenthesis, ParseErr::ExpectedRparen)?;
        Ok(expr)
    }
}
