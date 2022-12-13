pub mod compiler_error;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use compiler_error::CompilerError;
use ember::ir::instruction::IRInstruction;
use ember::ir::ir_generator::IRGenerator;
use ember::lexer::lex::Lexer;
use ember::parser::parse::Parser;
use ember::syntax::token::TokenInfo;
use ember::typechecker::typecheck::TypeChecker;
use ember::typechecker::typechecker_error::TypeCheckErr;
use std::fs;

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

    let data = fs::read_to_string(&options.path).expect("Unable to read file");
    let mut files = SimpleFiles::new();
    let file_id = files.add(&options.path, data);
    let file = files.get(file_id).unwrap();
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    match build(&options, file.source(), file_id) {
        Ok(_) => {}
        Err(errs) => {
            errs.iter()
                .map(|e| e.convert_to_diagnostic(file_id))
                .for_each(|d| term::emit(&mut writer.lock(), &config, &files, &d).unwrap());
            std::process::exit(0);
        }
    }
}

pub fn build(
    options: &CompilerOptions,
    input: &str,
    file_id: usize,
) -> Result<(), Vec<CompilerError>> {
    let tokens = Lexer::new_file(input, file_id).collect::<Vec<_>>();
    if options.emit_tokens {
        emit_tokens_to_file(&tokens);
    }

    let mut parser = Parser::new(tokens, input);
    let mut ast = match parser.parse_program() {
        Ok(stmts) => stmts,
        Err(errs) => {
            return Err(errs
                .iter()
                .map(|e| CompilerError::Parser(e.clone()))
                .collect::<Vec<_>>())
        }
    };

    let tc_errs = TypeChecker::check(&mut ast);
    if !tc_errs.is_empty() {
        return Err(tc_errs
            .iter()
            .map(|e: &TypeCheckErr| CompilerError::TypeCheck(e.clone()))
            .collect::<Vec<CompilerError>>());
    }

    if options.emit_ast {
        fs::create_dir_all(".\\emit").expect("Failed to create directory");
        fs::write(".\\emit\\ast.txt", format!("{ast:#?}")).expect("Unable to write file");
    }

    let mut ir_generator = IRGenerator::new();
    let generated_ir = ir_generator.gen_code(&ast).to_owned();
    if options.emit_ir {
        emit_ir_to_file(generated_ir);
    }

    Ok(())
}

fn emit_tokens_to_file(tokens: &[TokenInfo]) {
    fs::create_dir_all(".\\emit").expect("Failed to create directory");
    let token_string = tokens
        .iter()
        .map(|f| {
            format!(
                "{:>12}    {:?}",
                format!("{}..{}", f.span.start, f.span.end),
                f.token
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    fs::write(".\\emit\\tokens.txt", token_string).expect("Unable to write file");
}

fn emit_ir_to_file(generated_ir: Vec<IRInstruction>) {
    fs::create_dir_all(".\\emit").expect("Failed to create directory");
    let ir_string = generated_ir
        .iter()
        .map(|f| format!("{f}"))
        .collect::<Vec<_>>()
        .join("\n");
    fs::write(".\\emit\\ir.txt", ir_string).expect("Unable to write file");
}
