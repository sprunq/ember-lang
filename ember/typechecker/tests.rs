#[cfg(test)]
pub mod tests {
    use crate::{lexer::lex::Lexer, parser::parse::Parser, typechecker::typecheck::TypeChecker};

    pub fn expect_res(input: &str, expected: bool) {
        let mut parser = Parser::new(Lexer::new(input).collect(), input);
        let parse_res = parser.parse_program();
        match parse_res {
            Ok(mut program) => {
                let mut tc = TypeChecker::new(input);
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
        let a : i64 = 3;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_decl_int_error() {
        let input = "
        let a : i64 = false;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_decl_bool() {
        let input = "
        let a : bool = true;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_decl_bool_error() {
        let input = "
        let a  : bool = 0;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_identifier() {
        let input = "
        let a : bool = false;
        let b : bool = a;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_identifier_false() {
        let input = "
        let a : bool = false;
        let b : i64= a;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_infix() {
        let input = "
        let a : i64 = 0 + 1;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_infix_false() {
        let input = "
        let a : i64 = 0 + false;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_infix_idents() {
        let input = "
        let a :i64 = 0;
        let b :i64 = 1;
        let c :i64 = a + b;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_infix_idents_false() {
        let input = "
        let a : i64 = 0;
        let b : bool = false;
        let c : i64 = a + b;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_assign() {
        let input = "
        let a : i64 = 0;
        a += 20;
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_assign_false() {
        let input = "
        let a : i64 = 0;
        a += false;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_assign_ident_false() {
        let input = "
        let a : i64 = 0;
        let a : bool = false;
        a = b;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_if() {
        let input = "
        if (true == true){
            let i : i64 = 20;
        } else{
            let j : i64 = 0;
        };
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_if_false() {
        let input = "
        if (true == 0){
            let i : i64 = 20;
        } else{
            let j : i64 = 0;
        };
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_if_false_in_alternative() {
        let input = "
        if (true == true){
            let i : i64 = 20;
        } else{
            let j : i64 = false;
        };
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_if_with_idents() {
        let input = "
        let a : i64 = 0;
        let b : i64 = 1;
        if (a == b){
            let c : i64 = 20;
        } else{
            let j : bool = true;
        };
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_while() {
        let input = "
        while (true){
            let a : i64 = 20;
        };
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_while_false() {
        let input = "
        while (true == 1){
            let a : i64 = 20;
        };
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_while_idents_true() {
        let input = "
        let a : i64 = 0;
        let b : i64 = 0;
        while (a == b){
            let i : i64 = 20;
        };
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_while_idents_false() {
        let input = "
        let j : i64 = 0;
        let j : bool = false;
        while (a == b){
            let i : i64 = 20;
        };
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_scoped_ident() {
        let input = "
        if(true){
            let c : i64 = 4;
        };
        let d : i64 = c;
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_compex_if() {
        let input = "
        let a : i64 = 0;
        let b : bool = true;
        let c : bool = b;
        // comment
        while(b){
            a += 10;
            if(c != ((10000 == a) != b == (0 < -1))){
                let j : i64 = a / 20;
                let j : i64 = e;
            }
            a = a * -20;
        }
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_if_int() {
        let input = "
        if(0){}
        ";
        expect_res(input, false);
    }

    #[test]
    fn test_if_bool() {
        let input = "
        if(true){}
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_function_declaration() {
        let input = "
        fn main(a : i64, b : i64) -> i64 {
            return 0;
        }
        ";
        expect_res(input, true);
    }

    #[test]
    fn test_function_declaration_false() {
        let input = "
        fn main(a : i64, b : i64) -> i64 {
            return true;
        }
        ";
        expect_res(input, false);
    }
}
