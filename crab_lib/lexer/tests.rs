#[cfg(test)]
pub mod tests {
    use crate::lexer::{
        lex::Lexer,
        token::{Token, TokenInfo},
    };

    fn assert_lex_against(input: &str, expected: Vec<Token>) {
        let lexer: Vec<TokenInfo> = Lexer::new(input).collect();
        let mut idx = 0;
        for expected_token in expected.iter() {
            let actual_token = lexer[idx].token.clone();
            idx += 1;
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
        "#;
        let expected = vec![
            Token::I64,
            Token::Identifier,
            Token::Assign,
            Token::Number,
            Token::Semicolon,
            Token::I64,
            Token::Identifier,
            Token::Assign,
            Token::Number,
            Token::Semicolon,
        ];
        assert_lex_against(input, expected);
    }

    #[test]
    fn test_operator_lex() {
        let input = r#"
        +-/*5;
        5 < 10 > 5;
        "#;
        let expected = vec![
            Token::Plus,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Number,
            Token::Semicolon,
            Token::Number,
            Token::Lt,
            Token::Number,
            Token::Gt,
            Token::Number,
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }

    #[test]
    fn test_double_char_operators() {
        let input = r#"
        10 == 10;
        10 != 9;
        "#;
        let expected = vec![
            Token::Number,
            Token::Equal,
            Token::Number,
            Token::Semicolon,
            Token::Number,
            Token::NotEqual,
            Token::Number,
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }
}
