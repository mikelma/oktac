use clap::Parser;
use std::str::FromStr;

#[derive(Parser)]
// #[clap(setting = AppSettings::ColoredHelp)]
pub struct Opts {
    /// Paths of the source files to compile
    #[clap(required = true)]
    pub input: Vec<String>,
    /// Path to the output binary
    #[clap(short, long, default_value = "a.out")]
    pub output: String,
    /// Emit generated LLVM-IR to stdout
    #[clap(long, parse(from_occurrences))]
    pub emit_llvm: i32,
    /// Emit generated AST to stdout
    #[clap(long, parse(from_occurrences))]
    pub emit_ast: i32,
    /// Paths of the `.c` and `.`h files to include
    #[clap(short, long)]
    pub c_include: Option<Vec<String>>,

    /// Path to the temporal working directory
    #[clap(short, long, default_value = "/tmp/oktac-tmp")]
    pub tmp_dir: String,

    /// Path of the project's root
    #[clap(short, long)]
    pub root_path: Option<String>,
}

pub enum EmitOpts {
    LlvmIr,
    Ast,
}

impl FromStr for EmitOpts {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "llvm-ir" => Ok(EmitOpts::LlvmIr),
            "ast" => Ok(EmitOpts::Ast),
            _ => Err("Invalid emit parameter, valid values are: llvm-ir and ast"),
        }
    }
}
