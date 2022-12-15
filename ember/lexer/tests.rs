#[cfg(test)]
pub mod tests {
    use crate::{
        lexer::lex::Lexer,
        syntax::token::{Token, TokenInfo},
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
        int five = 5;
        int ten = 10;
        "#;
        let expected = vec![
            Token::Int,
            Token::Identifier,
            Token::Assign,
            Token::IntLiteral,
            Token::Semicolon,
            Token::Int,
            Token::Identifier,
            Token::Assign,
            Token::IntLiteral,
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
            Token::IntLiteral,
            Token::Semicolon,
            Token::IntLiteral,
            Token::Lt,
            Token::IntLiteral,
            Token::Gt,
            Token::IntLiteral,
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
            Token::IntLiteral,
            Token::Equal,
            Token::IntLiteral,
            Token::Semicolon,
            Token::IntLiteral,
            Token::NotEqual,
            Token::IntLiteral,
            Token::Semicolon,
        ];

        assert_lex_against(input, expected);
    }
}
