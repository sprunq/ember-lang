use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use ember::diagnostic_converter::convert_parse::build_parse_error_diagnostic;
use ember::diagnostic_converter::convert_typecheck::build_typecheck_error_diagnostic;
use ember::ir::ir_generator::IRGenerator;
use ember::lexer::lex::Lexer;
use ember::parser::parse::Parser;
use ember::typechecker::typecheck::TypeChecker;
use std::{fs, time::Instant};

pub struct CompilerOptions {
    pub path: String,
    pub measure_performance: bool,
    pub emit_tokens: bool,
    pub emit_ast: bool,
    pub emit_ir: bool,
}

fn main() {
    let options = CompilerOptions {
        path: String::from(".\\examples\\input.emb"),
        measure_performance: true,
        emit_tokens: true,
        emit_ast: true,
        emit_ir: true,
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
    let tokens = Lexer::new_file(file.source(), file_id).collect::<Vec<_>>();
    let lexing_elapsed = now.elapsed();

    if options.emit_tokens {
        fs::create_dir_all(".\\emit").expect("Failed to create directory");
        let token_string = tokens
            .iter()
            .map(|f| format!("{:?}\t\t{:?}", f.span, f.token))
            .collect::<Vec<_>>()
            .join("\n");
        fs::write(".\\emit\\tokens.txt", token_string).expect("Unable to write file");
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

    /*
    let now = Instant::now();
    let mut ir_generator = IRGenerator::new();
    let generated_ir = ir_generator.gen_code(&ast).to_owned();
    let ir_gen_elapsed = now.elapsed();

    if options.emit_ir {
        fs::create_dir_all(".\\emit").expect("Failed to create directory");
        let ir_string = generated_ir
            .iter()
            .map(|f| format!("{f}"))
            .collect::<Vec<_>>()
            .join("\n");
        fs::write(".\\emit\\ir.txt", ir_string).expect("Unable to write file");
    }

    if options.measure_performance {
        println!("\nPerformance:");
        println!("- Lexing \t{lexing_elapsed:.2?}");
        println!("- Parsing \t{parsing_elapsed:.2?}");
        println!("- Typecheck \t{typecheck_elapsed:.2?}");
        println!("- IR Gen \t{ir_gen_elapsed:.2?}");
        println!(
            "- Total \t{:.5?}s",
            (lexing_elapsed + parsing_elapsed + typecheck_elapsed + ir_gen_elapsed).as_secs_f64()
        );
    }
    */
}
