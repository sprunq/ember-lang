#[cfg(test)]
pub mod tests {
    use crate::{lexer::lex::Lexer, parser::parse::Parser, typechecker::typecheck::TypeChecker};

    pub fn check_and_expect_result(input: &str, should_fail: bool) {
        let mut parser = Parser::new(Lexer::new(input).collect(), input);
        let parse_res = parser.parse_program();
        match parse_res {
            Ok(mut program) => {
                let tc = TypeChecker::check(&mut program);
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
        def main() {
            let a : int = 20;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_decl_bool() {
        let input = "
        def main() {
            let a : bool = false;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_decl_mismatch() {
        let input = "
        def main() {
            let a : int = false;
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_decl_complex() {
        let input = "
        def main() {
            let a : bool = (20*5) == 100;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_var_assign() {
        let input = "
        def main() {
            let a : int = 20;
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
        def main() {
            let a : int = 20;
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
        def main() {
            let a : int = 20;
            a + false;
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_type_iterop_cond() {
        let input = "
        def main() {
            if(true){
            }
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_type_iterop_cond_int() {
        let input = "
        def main() {
            if(0){
            }
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_type_iterop_cond_comp() {
        let input = "
        def main() {
            if(200 == 200){
            }
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_nomain() {
        let input = "
        def not_main() {
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_double_fun_decl() {
        let input = "
        def not_main() {
        }
        def not_main() {
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_param() {
        let input = "
        def main(a : int) {
            let b : int = a;
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_param_mismatch() {
        let input = "
        def main(a : int) {
            let b : bool = a;
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_invoc() {
        let input = "
        def main() {
            function_to_call(20, true);
        }
        
        def function_to_call(a: int, b : bool) {
        }
        ";
        check_and_expect_result(input, false);
    }

    #[test]
    fn test_invoc_too_many_args() {
        let input = "
        def main() {
            function_to_call(20, true, 0);
        }
        
        def function_to_call(a: int, b : bool) {
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_func_decl_return() {
        let input = "
        def main() {
            function_to_call(20, true, 0);
        }
        
        def function_to_call(a: int, b : bool) -> int {
            return 0;
        }
        ";
        check_and_expect_result(input, true);
    }

    #[test]
    fn test_func_decl_wrong_return() {
        let input = "
        def main() {
            function_to_call(20, true, 0);
        }
        
        def function_to_call(a: int, b : bool) -> int {
            return false;
        }
        ";
        check_and_expect_result(input, true);
    }
}
