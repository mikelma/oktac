use clap::Parser as ClapParser;
use std::str::FromStr;

#[derive(ClapParser)]
// #[clap(setting = AppSettings::ColoredHelp)]
pub struct Opts {
    /// Paths of the source files to compile
    #[clap(required = true)]
    pub input: Vec<String>,

    /// Path to the output binary
    #[clap(short, long, default_value = "a.out")]
    pub output: String,

    /// Output for the compiler to emit: llvm-ir, ast or ast-dbg 
    #[clap(long)]
    pub emit: Option<EmitOpts>,

    /// Paths of the `.c` and `.`h files to include
    #[clap(short, long)]
    pub c_include: Option<Vec<String>>,

    /// Path to the temporal working directory
    #[clap(short, long, default_value = "/tmp/oktac-tmp")]
    pub tmp_dir: String,

    /// Path to the project's root
    #[clap(short, long)]
    pub root_path: Option<String>,
}

#[derive(PartialEq, Eq)]
pub enum EmitOpts {
    LlvmIr,
    Ast,
    AstDebug,
}

impl FromStr for EmitOpts {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "llvm-ir" => Ok(EmitOpts::LlvmIr),
            "ast" => Ok(EmitOpts::Ast),
            "ast-dbg" => Ok(EmitOpts::AstDebug),
            _ => Err("Invalid emit parameter, valid values are: llvm-ir, ast and ast-dbg"),
        }
    }
}

impl EmitOpts {
    pub fn ast(&self) -> bool {
        matches!(self, EmitOpts::Ast | EmitOpts::AstDebug) 
    }
}
