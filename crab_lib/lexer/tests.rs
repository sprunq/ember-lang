#[cfg(test)]
pub mod tests {
    use crate::lexer::{lex::Lexer, token::Token};

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
            Token::Number("5".to_string()),
            Token::Semicolon,
            Token::I64,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Number("10".to_string()),
            Token::Semicolon,
        ];
        assert_lex_against(input, expected);
    }

    #[test]
    fn test_operator_lex() {
        let input = r#"
        +-/*5;
        5 < 10 > 5;
        "#
        .to_string();
        let expected = vec![
            Token::Plus,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Number("5".to_string()),
            Token::Semicolon,
            Token::Number("5".to_string()),
            Token::Lt,
            Token::Number("10".to_string()),
            Token::Gt,
            Token::Number("5".to_string()),
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
            Token::Number("10".to_string()),
            Token::Equal,
            Token::Number("10".to_string()),
            Token::Semicolon,
            Token::Number("10".to_string()),
            Token::NotEqual,
            Token::Number("9".to_string()),
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }
}
