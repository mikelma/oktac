use super::ast::*;
use std::fmt;

/// Logger message.
pub struct LogMesg<T> {
    mtype: MessageType,
    name: Option<T>,
    location: Option<usize>,
    lines: Option<String>,
    cause: Option<T>,
    help: Option<T>,
}

pub enum MessageType {
    Warning,
    Error,
}

impl<T> LogMesg<T> where T: fmt::Display {
    pub fn warn() -> LogMesg<T> {
        LogMesg {
            mtype: MessageType::Warning,
            name: None,
            location: None,
            lines: None,
            cause: None,
            help: None,
        } 
    }

    pub fn name(mut self, name: T) -> LogMesg<T> {
        self.name = Some(name);
        self
    }

    pub fn cause(mut self, cause: T) -> LogMesg<T> {
        self.cause = Some(cause);
        self
    }

    pub fn help(mut self, help: T) -> LogMesg<T> {
        self.help = Some(help);
        self
    }

    pub fn lines(mut self, lines: &str) -> LogMesg<T> {
        self.lines = Some(lines.to_string());
        self
    }

    pub fn location(mut self, line_num: usize) -> LogMesg<T> {
        self.location = Some(line_num);
        self
    }

    pub fn send(&self) -> Result<(), &'static str> {
        let mut msg = format!("[{}]", self.mtype);

        if let Some(line) = self.location {
            msg = format!("{} (line {})", msg, line);
        }

        msg = match &self.cause {
            Some(c) => format!("{} {}", msg, c),
            None => return Err("Missing cause component for LogMesg"),
        };

        msg = match &self.name {
            Some(name) => format!("{}: {}", msg, name),
            None => return Err("Missing name component for LogMesg"),
        };

        if let Some(lines) = &self.lines {
            msg.push_str("\n  |");
            for line in lines.lines() {
                msg = format!("{}\n  | {}", msg, line);
            }
            msg.push_str("\n  |");
        }

        if let Some(help) = &self.help {
            msg = format!("{}\n    => help: {}", msg, help);
        }

        eprintln!("{}\n", msg);
        Ok(())
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
