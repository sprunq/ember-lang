use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use ember::diagnostic_converter::convert_parse::build_parse_error_diagnostic;
use ember::diagnostic_converter::convert_typecheck::build_typecheck_error_diagnostic;
use ember::lexer::lex::Lexer;
use ember::parser::parse::Parser;
use ember::typechecker::typecheck::TypeChecker;
use std::{fs, time::Instant};

pub struct CompilerOptions {
    pub path: String,
    pub measure_performance: bool,
    pub emit_ast: bool,
    pub emit_tokens: bool,
}

fn main() {
    let options = CompilerOptions {
        path: String::from(".\\examples\\input.em"),
        measure_performance: true,
        emit_ast: true,
        emit_tokens: true,
    };

    run(options);
}

pub fn run(options: CompilerOptions) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let data = fs::read_to_string(&options.path).expect("Unable to read file");

    let mut files = SimpleFiles::new();
    let file_id = files.add(&options.path, data);

    let file = files.get(file_id).unwrap();

    let now = Instant::now();
    let tokens = Lexer::new_file(file.source(), file_id).collect();
    let lexing_elapsed = now.elapsed();

    if options.emit_tokens {
        fs::create_dir_all(".\\emit").expect("Failed to create directory");
        fs::write(".\\emit\\tokens.txt", format!("{tokens:#?}")).expect("Unable to write file");
    }

    let mut parser = Parser::new(tokens, file.source());
    let now = Instant::now();
    let parse_res = parser.parse_program();
    let parsing_elapsed = now.elapsed();

    if let Err(error) = parse_res {
        let diagnostic = build_parse_error_diagnostic(error, file_id);
        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
        std::process::exit(0);
    }

    let ast = parse_res.unwrap();

    if options.emit_ast {
        fs::create_dir_all(".\\emit").expect("Failed to create directory");
        fs::write(".\\emit\\ast.txt", format!("{ast:#?}")).expect("Unable to write file");
    }

    let now = Instant::now();
    let typechecker = TypeChecker::new(file.source());
    let typecheck_result = typechecker.typecheck(&ast);
    let typecheck_elapsed = now.elapsed();

    if let Err(error) = typecheck_result {
        let diagnostic = build_typecheck_error_diagnostic(error, file_id);
        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
        std::process::exit(0);
    }

    if options.measure_performance {
        println!("\nPerformance:");
        println!("- Lexing \t{lexing_elapsed:.2?}");
        println!("- Parsing \t{parsing_elapsed:.2?}");
        println!("- Typecheck \t{typecheck_elapsed:.2?}");
    }
}
