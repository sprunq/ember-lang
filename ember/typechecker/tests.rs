#[cfg(test)]
pub mod tests {
    use crate::{lexer::lex::Lexer, parser::parse::Parser, typechecker::typecheck::TypeChecker};

    pub fn check_and_expect_result(input: &str, should_fail: bool) {
        let mut parser = Parser::new(Lexer::new(input).collect(), input);
        let parse_res = parser.parse_program();
        match parse_res {
            Ok(program) => {
                let tc = TypeChecker::check(&program);
                if !tc.is_empty() {
                    println!("{:?}", tc);
                }
                assert_eq!(!tc.is_empty(), should_fail);
            }
            Err(err) => panic!("Got error: {:?}", err),
        }
    }

    #[test]
    fn test_decl_int() {
        let input = "
        fn main() {
            let a : i64 = 20;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_decl_bool() {
        let input = "
        fn main() {
            let a : bool = false;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_decl_mismatch() {
        let input = "
        fn main() {
            let a : i64 = false;
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_decl_complex() {
        let input = "
        fn main() {
            let a : bool = (20*5) == 100;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_var_assign() {
        let input = "
        fn main() {
            let a : i64 = 20;
            a += 20;
            a = 10;
            a -= 100;
            a *= 2;
            a /= 20;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_type_interop() {
        let input = "
        fn main() {
            let a : i64 = 20;
            a + 20;
            a - 20;
            a * 20;
            a / 20;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_type_interop_fail() {
        let input = "
        fn main() {
            let a : i64 = 20;
            a + false;
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_type_iterop_cond() {
        let input = "
        fn main() {
            if(true){
            }
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_type_iterop_cond_int() {
        let input = "
        fn main() {
            if(0){
            }
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_type_iterop_cond_comp() {
        let input = "
        fn main() {
            if(200 == 200){
            }
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_nomain() {
        let input = "
        fn not_main() {
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_double_fun_decl() {
        let input = "
        fn not_main() {
        }
        fn not_main() {
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_param() {
        let input = "
        fn main(a : i64) {
            let b : i64 = a;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_param_mismatch() {
        let input = "
        fn main(a : i64) {
            let b : bool = a;
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_invoc() {
        let input = "
        fn main() {
            function_to_call(20, true);
        }
        
        fn function_to_call(a: i64, b : bool) {
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_invoc_too_many_args() {
        let input = "
        fn main() {
            function_to_call(20, true, 0);
        }
        
        fn function_to_call(a: i64, b : bool) {
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_func_decl_return() {
        let input = "
        fn main() {
            function_to_call(20, true, 0);
        }
        
        fn function_to_call(a: i64, b : bool) -> i64 {
            return 0;
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_func_decl_wrong_return() {
        let input = "
        fn main() {
            function_to_call(20, true, 0);
        }
        
        fn function_to_call(a: i64, b : bool) -> i64 {
            return false;
        }
        ";
        check_and_expect_result(input, true);
    }
}
