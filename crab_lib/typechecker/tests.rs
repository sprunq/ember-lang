#[cfg(test)]
pub mod tests {
    use crate::{lexer::lex::Lexer, parser::parse::Parser, typechecker::typecheck::TypeChecker};

    pub fn expect_res(input: &str, expected: bool) {
        let mut parser = Parser::new(Lexer::tokenize_all_collect(input), input);
        let parse_res = parser.parse_program();
        match parse_res {
            Ok(mut program) => {
                let tc = TypeChecker {
                    input: input.to_string(),
                };
                let res = tc.typecheck(&mut program);
                if res.is_err() {
                    println!("{:#?}", res);
                }
                assert_eq!(res.is_ok(), expected);
            }
            Err(err) => panic!("Got error: {:?}", err),
        }
    }

    #[test]
    fn test_decl_int() {
        let input = "
        i64 a = 3;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_decl_int_error() {
        let input = "
        i64 a = false;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_decl_bool() {
        let input = "
        bool a = true;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_decl_bool_error() {
        let input = "
        bool a = 0;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_identifier() {
        let input = "
        bool a = false;
        bool b = a;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_identifier_false() {
        let input = "
        bool a = false;
        i64 b = a;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_infix() {
        let input = "
        i64 a = 0 + 1;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_infix_false() {
        let input = "
        i64 a = 0 + false;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_infix_idents() {
        let input = "
        i64 a = 0;
        i64 b = 1;
        i64 c = a + b;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_infix_idents_false() {
        let input = "
        i64 a = 0;
        bool b = false;
        i64 c = a + b;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_assign() {
        let input = "
        i64 a = 0;
        a += 20;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_assign_false() {
        let input = "
        i64 a = 0;
        a += false;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_assign_ident_false() {
        let input = "
        i64 a = 0;
        bool b = false;
        a = b;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_if() {
        let input = "
        if (true == true){
            i64 i = 20;
        } else{
            i64 j = 0;
        };
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_if_false() {
        let input = "
        if (true == 0){
            i64 i = 20;
        } else{
            i64 j = 0;
        };
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_if_false_in_alternative() {
        let input = "
        if (true == true){
            i64 i = 20;
        } else{
            i64 j = false;
        };
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_if_with_idents() {
        let input = "
        i64 a = 0;
        i64 b = 1;
        if (a == b){
            i64 i = 20;
        } else{
            bool j = true;
        };
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_while() {
        let input = "
        while (true){
            i64 i = 20;
        };
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_while_false() {
        let input = "
        while (true == 1){
            i64 i = 20;
        };
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_while_idents_true() {
        let input = "
        i64 a = 0;
        i64 b = 0;
        while (a == b){
            i64 i = 20;
        };
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_while_idents_false() {
        let input = "
        i64 a = 0;
        bool b = false;
        while (a == b){
            i64 i = 20;
        };
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_scoped_ident() {
        let input = "
        if(true){
            i64 c = 4;
        };
        i64 d = c;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_compex_if() {
        let input = "
        i64 a = 0;
        bool b = true;
        bool c = b;
        // comment
        while(b){
            a += 10;
            if(c != ((10000 == a) != b == (0 < -1))){
                i64 e = a / 20;
                i64 d = e;
            };
            a = a * -20;
        };
        ";
        expect_res(input, false);
    }
}
