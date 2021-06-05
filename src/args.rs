use clap::{AppSettings, Clap};
use std::str::FromStr;

#[derive(Clap)]
#[clap(version = "1.0", author = "Kevin K. <kbknapp@gmail.com>")]
#[clap(setting = AppSettings::ColoredHelp)]

pub struct Opts {
    #[clap(short, long, required=true)]
    pub input: String,
    #[clap(short, long, default_value="llvm-ir")]
    pub emit: EmitOpts,
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
            _ => Err("Invalid emit value, valid values are: llvm-ir and ast"),
        }
    }
}
