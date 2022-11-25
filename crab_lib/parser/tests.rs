#[cfg(test)]
pub mod tests {
    use crate::ast::expression::TypedExpr;
    use crate::ast::ty::Type;
    use crate::ast::{expression::Expr, infix::Infix, statement::Stmt};
    use crate::lexer::lexer::Lexer;
    use crate::parser::parser::Parser;

    fn check_str_str_eq(intput_output: Vec<(&str, &str)>) {
        for (input, expected) in intput_output {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);
            let parse_res = parser.parse_program();

            match parse_res {
                Ok(program) => assert_eq!(format!("{}", program), expected),
                Err(err) => panic!("Got error: {:?}", err),
            }
        }
    }

    #[test]
    fn test_decl_stmt() {
        let input = "
        i64 x = 5;
        i64 y = 7;
        i64 foobar = y;
        ";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        let expected = vec![
            Stmt::Declaration {
                ty: Type::I64,
                ident: "x".to_string(),
                value: TypedExpr {
                    ty: Some(Type::I64),
                    expr: Expr::IntegerLiteral(5),
                },
            },
            Stmt::Declaration {
                ty: Type::I64,
                ident: "y".to_string(),
                value: TypedExpr {
                    ty: Some(Type::I64),
                    expr: Expr::IntegerLiteral(7),
                },
            },
            Stmt::Declaration {
                ty: Type::I64,
                ident: "foobar".to_string(),
                value: TypedExpr {
                    ty: None,
                    expr: Expr::Identifier("y".to_string()),
                },
            },
        ];

        assert_eq!(expected, program.unwrap().sequence);
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";
        let lexer = Lexer::new(input.to_owned());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![Stmt::Expression {
            expr: TypedExpr {
                ty: Some(Type::I64),
                expr: Expr::IntegerLiteral(5),
            },
        }];

        assert_eq!(expected, program.unwrap().sequence);
    }

    #[test]
    fn test_infix_expression() {
        let tests = vec![
            ("5 + 5;", 5, Infix::Plus, 5),
            ("5 - 5;", 5, Infix::Minus, 5),
            ("5 * 5;", 5, Infix::Asterisk, 5),
            ("5 / 5;", 5, Infix::Slash, 5),
            ("5 > 5;", 5, Infix::Gt, 5),
            ("5 < 5;", 5, Infix::Lt, 5),
            ("5 == 5;", 5, Infix::Eq, 5),
            ("5 != 5;", 5, Infix::NotEq, 5),
        ];
        for (input, left, operator, right) in tests {
            let lexer = Lexer::new(input.to_owned());
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();

            assert_eq!(
                program.unwrap().sequence,
                vec![Stmt::Expression {
                    expr: TypedExpr::new(Expr::Infix(
                        operator,
                        Box::new(TypedExpr {
                            ty: Some(Type::I64),
                            expr: Expr::IntegerLiteral(left)
                        }),
                        Box::new(TypedExpr {
                            ty: Some(Type::I64),
                            expr: Expr::IntegerLiteral(right)
                        })
                    ))
                }]
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
            ("!-a;", "(!(-a));"),
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
