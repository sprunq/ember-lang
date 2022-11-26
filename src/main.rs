use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use crab_lib::diagnostic_converter::converter_parse_error::build_parse_error_diagnostic;
use crab_lib::{lexer::lex::Lexer, parser::parse::Parser};
use std::{fs, time::Instant};

fn main() {
    let path = String::from(".\\examples\\compile.crab");
    let measure_perf = true;

    run(&path, measure_perf);
}

pub fn run(path: &String, measure_time: bool) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let data = fs::read_to_string(path).expect("Unable to read file");

    let mut files = SimpleFiles::new();
    let file_id = files.add(path, data);

    let file = files.get(file_id).unwrap();
    let input = file.source().clone();
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let now = Instant::now();
    let parse_res = parser.parse_program();
    let parsing_elapsed = now.elapsed();

    match parse_res {
        Err(err) => {
            let diagnostic = build_parse_error_diagnostic(err, file_id);
            term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            std::process::exit(0);
        }
        Ok(ast) => println!("{:#?}", ast),
    }

    if measure_time {
        println!("\nExection times:");
        println!("Parsing time: {:.2?}", parsing_elapsed);
    }
}
