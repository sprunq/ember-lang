use crab_lib::{lexer::lex::Lexer, parser::parse::Parser};
use std::{fs, time::Instant};

fn main() {
    let path = String::from(".\\examples\\compile.crab");
    let measure_perf = true;

    run(&path, measure_perf);
}

pub fn run(path: &String, measure_time: bool) {
    let data = fs::read_to_string(path).expect("Unable to read file");
    let lexer = Lexer::new(data);
    let mut parser = Parser::new(lexer);

    let now = Instant::now();
    let parse_res = parser.parse_program();
    let parsing_elapsed = now.elapsed();

    match parse_res {
        Err(ref err) => {
            println!("Oh Crab! I encountered an error during parsing:\n{:#}", err);
            std::process::exit(0);
        }
        Ok(ast) => println!("{:#?}", ast),
    }

    if measure_time {
        println!("\nExection times:");
        println!("Parsing time: {:.2?}", parsing_elapsed);
    }
}
