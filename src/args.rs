use clap::Parser as ClapParser;
use target_lexicon::{Triple, HOST};

use std::{
    fmt::{self, Display},
    str::FromStr,
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

    /// Target triple to compile for. Defaults to host's triple.
    #[clap(long, default_value_t = CompileTarget::default())]
    pub target: CompileTarget,

    /// Print extra information to stdout
    #[clap(short, long)]
    pub verbose: bool,

    /// Shhh, be silent
    #[clap(short, long)]
    pub quiet: bool,
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

/// Wrapper for `target_lexicon::Triple` that implements default and whose error type
/// in `FromStr` is `Err(String)`.
pub struct CompileTarget(Triple);

impl CompileTarget {
    pub fn triple(&self) -> &Triple {
        &self.0
    }
}

impl Default for CompileTarget {
    fn default() -> Self {
        CompileTarget(HOST)
    }
}

impl FromStr for CompileTarget {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match Triple::from_str(s) {
            Ok(t) => Ok(CompileTarget(t)),
            Err(e) => Err(format!("{}", e)),
        }
    }
}

impl Display for CompileTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
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
