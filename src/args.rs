use clap::Parser as ClapParser;
use std::{
    str::FromStr, 
    fmt::{self, Display},
};

#[derive(ClapParser)]
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

    /// Optimization level 
    #[clap(short = 'O', long, default_value = "2")]
    pub opt_level: OptLevel,
}

#[derive(PartialEq, Eq)]
pub enum EmitOpts {
    LlvmIr,
    Ast,
    AstDebug,
}

#[derive(PartialEq, Eq)]
pub enum OptLevel {
    None,
    O1,
    O2,
    O3,
    Ofast,
}

impl FromStr for OptLevel {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(OptLevel::None),
            "1" => Ok(OptLevel::O1),
            "2" => Ok(OptLevel::O2),
            "3" => Ok(OptLevel::O3),
            "fast" => Ok(OptLevel::Ofast),
            _ => Err("Invalid parameter, valid opt-level values are: O0, O1, O2, O3 and Ofast"),
        }
    }
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

impl Display for OptLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OptLevel::None => write!(f, "O0"),
            OptLevel::O1 => write!(f, "O1"),
            OptLevel::O2 => write!(f, "O2"),
            OptLevel::O3 => write!(f, "O3"),
            OptLevel::Ofast => write!(f, "Ofast"),
        }
    }
}

impl EmitOpts {
    pub fn ast(&self) -> bool {
        matches!(self, EmitOpts::Ast | EmitOpts::AstDebug)
    }
}
