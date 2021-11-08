use console::style;

use std::fmt;

use super::current_unit_status;

#[derive(Debug)]
/// Logger message.
pub struct LogMesg {
    mtype: MessageType,
    filename: String,
    name: Option<String>,
    location: Option<usize>,
    lines: Option<String>,
    cause: Option<String>,
    help: Option<String>,
}

#[derive(Debug)]
pub enum MessageType {
    Warning,
    Error,
}

impl LogMesg {
    pub fn warn() -> LogMesg {
        LogMesg {
            mtype: MessageType::Warning,
            // NOTE: Filename will be determined when `send` method is called
            filename: "".into(),
            name: None,
            location: None,
            lines: None,
            cause: None,
            help: None,
        }
    }

    pub fn err() -> LogMesg {
        LogMesg {
            mtype: MessageType::Error,
            filename: "".into(),
            name: None,
            location: None,
            lines: None,
            cause: None,
            help: None,
        }
    }

    pub fn name(mut self, name: &str) -> LogMesg {
        self.name = Some(name.into());
        self
    }

    pub fn cause(mut self, cause: String) -> LogMesg {
        self.cause = Some(cause);
        self
    }

    pub fn help(mut self, help: String) -> LogMesg {
        self.help = Some(help);
        self
    }

    pub fn lines(mut self, lines: &str) -> LogMesg {
        self.lines = Some(lines.to_string());
        self
    }

    pub fn location(mut self, line_num: usize) -> LogMesg {
        self.location = Some(line_num);
        self
    }

    pub fn send(mut self) -> Result<(), &'static str> {
        self.filename = current_unit_status!().lock().unwrap().filename.clone();
        match self.mtype {
            MessageType::Error => current_unit_status!().lock().unwrap().errors.push(self),
            MessageType::Warning => current_unit_status!().lock().unwrap().warnings.push(self),
        }

        Ok(())
    }

    pub fn get_name(&self) -> Option<&String> {
        self.name.as_ref()
    }
}

pub enum SyntaxWarn {
    UnreachableCode,
}

impl fmt::Display for SyntaxWarn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxWarn::UnreachableCode => write!(f, "Unreachable code"),
        }
    }
}

impl fmt::Display for MessageType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MessageType::Warning => write!(f, "W"),
            MessageType::Error => write!(f, "E"),
        }
    }
}

impl fmt::Display for LogMesg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut msg = match self.location {
            Some(l) => format!("[[{}] {}:{}]", self.mtype, self.filename, l),
            None => format!("[[{}] {}]", self.mtype, self.filename),
        };

        msg = match self.mtype {
            MessageType::Warning => format!("{}", style(msg).bold().yellow()),
            MessageType::Error => format!("{}", style(msg).bold().red()),
        };

        msg = match &self.name {
            Some(name) => format!("{} {}:", msg, style(name).bold()),
            None => panic!("Missing name component for LogMesg"),
        };

        msg = match &self.cause {
            Some(c) => format!("{} {}", msg, c),
            None => panic!("Missing cause component for LogMesg"),
        };

        if let Some(lines) = &self.lines {
            msg += &style("\n  │").blue().to_string();
            for line in lines.lines() {
                msg = format!("{}\n  {} {}", msg, style("│").blue(), line);
            }
            msg += &style("\n  │").blue().to_string();
        }

        if let Some(help) = &self.help {
            msg = format!(
                "{}{} {}: {}",
                msg,
                style("\n  └──").blue(),
                style("help").bold().blue(),
                help
            );
        }

        write!(f, "{}", msg)
    }
}

/*
pub trait UserMessage {
    fn name(&self) -> String;
    // fn cause(&self) -> String;
    // fn location(&self) -> (String, usize, usize, String);
    fn help(&self) -> Option<String>;

    fn mtype(&self) -> MessageType;

    fn send(&self) {
        match self.help() {
            Some(help) =>  eprintln!("[{}] {}\n\t-> help: {}",
                                     self.mtype(), self.name(), help),
            None => eprintln!("[{}] {}",
                              self.mtype(), self.name()),
        }
    }
}
*/

/*
pub fn syntax_check(ast: &mut Vec<AstNode>) {
    for node in ast {
        check(node);
    }
}

fn check(ast: &mut AstNode) {
    match ast {
        AstNode::Stmts(stmts) => {
            let trunc = stmts.iter()
                .take_while(|&x| {
                    if let AstNode::ReturnExpr(_) = x {
                        SyntaxWarn::UnreachableCode("return".into()).send();
                        true
                    } else if *x == AstNode::BreakExpr  {
                        SyntaxWarn::UnreachableCode("break".into()).send();
                        true
                    } else { false }
                })
                .count();

            if trunc < stmts.len() {
                stmts.truncate(trunc + 1);
            }
        },
        _ => (),
    }
}
*/
