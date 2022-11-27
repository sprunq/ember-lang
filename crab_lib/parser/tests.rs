#[cfg(test)]
pub mod tests {
    use crate::{
        ast::{
            expression::{Expr, Node},
            infix::InfixOp,
            statement::Stmt,
            ty::Type,
            typed_expression::TypedExpr,
        },
        lexer::lex::Lexer,
        parser::parse::Parser,
    };

    pub fn check_str_str_eq(intput_output: Vec<(&str, &str)>) {
        for (input, expected) in intput_output {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let parse_res = parser.parse_program();

            match parse_res {
                Ok(program) => assert_eq!(format!("{}", program), expected),
                Err(err) => panic!("Got error: {:#?}", err),
            }
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests = vec![
            ("5 + 5;", 5, InfixOp::Plus, 5),
            ("5 - 5;", 5, InfixOp::Minus, 5),
            ("5 * 5;", 5, InfixOp::Asterisk, 5),
            ("5 / 5;", 5, InfixOp::Slash, 5),
            ("5 > 5;", 5, InfixOp::Gt, 5),
            ("5 < 5;", 5, InfixOp::Lt, 5),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            assert_eq!(
                program.unwrap().sequence,
                Stmt::Sequence {
                    statements: Box::new(vec![Stmt::Expression {
                        expr: Node::new(
                            TypedExpr::new(Expr::Infix {
                                op: operator,
                                left: Node::new_boxed(
                                    TypedExpr {
                                        ty: Some(Type::I64),
                                        expr: Expr::IntegerLiteral(left, 0..1)
                                    },
                                    0..1
                                ),
                                right: Node::new_boxed(
                                    TypedExpr {
                                        ty: Some(Type::I64),
                                        expr: Expr::IntegerLiteral(right, 4..5)
                                    },
                                    4..5
                                )
                            }),
                            0..5
                        )
                    }])
                }
            );
        }
    }

    #[test]
    fn test_infix_expression_2() {
        let tests = vec![
            ("5 == 5;", 5, InfixOp::Eq, 5),
            ("5 != 5;", 5, InfixOp::NotEq, 5),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            assert_eq!(
                program.unwrap().sequence,
                Stmt::Sequence {
                    statements: Box::new(vec![Stmt::Expression {
                        expr: Node::new(
                            TypedExpr::new(Expr::Infix {
                                op: operator,
                                left: Node::new_boxed(
                                    TypedExpr {
                                        ty: Some(Type::I64),
                                        expr: Expr::IntegerLiteral(left, 0..1)
                                    },
                                    0..1
                                ),
                                right: Node::new_boxed(
                                    TypedExpr {
                                        ty: Some(Type::I64),
                                        expr: Expr::IntegerLiteral(right, 5..6)
                                    },
                                    5..6
                                )
                            }),
                            0..6
                        )
                    }])
                }
            );
        }
    }

    #[test]
    fn test_identifier() {
        let intput_output = vec![("foobar;", "foobar;")];
        check_str_str_eq(intput_output);
    }

    #[test]
    fn test_operator_precedence() {
        let intput_output = vec![
            ("-a * b;", "((-a) * b);"),
            ("a + b + c;", "((a + b) + c);"),
            ("a + b - c;", "((a + b) - c);"),
            ("a * b * c;", "((a * b) * c);"),
            ("a * b / c;", "((a * b) / c);"),
            ("a + b / c;", "(a + (b / c));"),
            ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5;", "(3 + 4);((-5) * 5);"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2;", "((5 + 5) * 2);"),
            ("2 / (5 + 5);", "(2 / (5 + 5));"),
            ("-(5 + 5);", "(-(5 + 5));"),
            (
                "if (x < y) { x; } else { y; };",
                "if (x < y) { x; } else { y; };",
            ),
        ];

        check_str_str_eq(intput_output);
    }
}
