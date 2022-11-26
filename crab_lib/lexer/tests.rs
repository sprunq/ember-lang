#[cfg(test)]
pub mod tests {
    use crate::lexer::{lex::Lexer, literal::Literal, token::Token};

    fn assert_lex_against(input: String, expected: Vec<Token>) {
        let mut lexer = Lexer::new(input);
        for expected_token in expected.iter() {
            let actual_token = lexer.next_token().token;
            assert_eq!(
                *expected_token, actual_token,
                "expected {} found {}",
                expected_token, actual_token
            );
        }
    }

    #[test]
    fn test_var_lex() {
        let input = r#"
        i64 five = 5;
        i64 ten = 10;
        "#
        .to_string();
        let expected = vec![
            Token::I64,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Literal(Literal::Integer(5)),
            Token::Semicolon,
            Token::I64,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Literal(Literal::Integer(10)),
            Token::Semicolon,
        ];
        assert_lex_against(input, expected);
    }

    #[test]
    fn test_operator_lex() {
        let input = r#"
        +-!/*5;
        5 < 10 > 5;
        "#
        .to_string();
        let expected = vec![
            Token::Plus,
            Token::Minus,
            Token::Bang,
            Token::Slash,
            Token::Asterisk,
            Token::Literal(Literal::Integer(5)),
            Token::Semicolon,
            Token::Literal(Literal::Integer(5)),
            Token::Lt,
            Token::Literal(Literal::Integer(10)),
            Token::Gt,
            Token::Literal(Literal::Integer(5)),
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }

    #[test]
    fn test_double_char_operators() {
        let input = r#"
        10 == 10;
        10 != 9;
        "#
        .to_string();
        let expected = vec![
            Token::Literal(Literal::Integer(10)),
            Token::Equal,
            Token::Literal(Literal::Integer(10)),
            Token::Semicolon,
            Token::Literal(Literal::Integer(10)),
            Token::NotEqual,
            Token::Literal(Literal::Integer(9)),
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }
}
