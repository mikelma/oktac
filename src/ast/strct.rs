use pest::iterators::Pair;
use console::style;

use super::{parser::*, *};
use crate::{VarType, ST, LogMesg};

pub fn parse_struct_decl(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let name = inner.next().unwrap().as_str().to_string();

    let members = inner.map(|p| {
        let mut param = p.into_inner();
        let ty = expr::parse_var_type(param.next().unwrap());    
        let id = param.next().unwrap().as_str();    
        (id.into(), ty)
    }).collect::<Vec<(String, VarType)>>();

    // register the struct 
    let res = ST
        .lock()
        .unwrap()
        .record_struct(&name, members.clone());

    if let Err(e) = res {
        e.lines(pair.as_str())
         .location(pair.as_span().start_pos().line_col().0)
         .send().unwrap();
    }
    
    AstNode::StructDef { name, members }
}

pub fn parse_struct_value(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let struct_name = inner.next().unwrap().as_str().to_string();

    // check if the struct type exists
    let true_members = match ST.lock().unwrap().search_struct(&struct_name) {
        Ok(m) => Some(m),
        Err(e) => {
            e.lines(pair.as_str())
             .location(pair.as_span().start_pos().line_col().0)
             .lines(pair.as_str())
             .send()
             .unwrap();
            None
        },
    };

    // pase member of the struct
    let unordered_members = inner.map(|m| {
        let mut member = m.into_inner();

        let memb_name = member.next().unwrap().as_str().to_string();

        // extract the "true" type of this member (extracted from the struct definition)
        let true_ty = match &true_members {
            Some(membs) => membs.iter().find(|(name, _)| *name == memb_name).map(|v| v.1.clone()),
            None => None,
        };

        // check if the member really exits in the struct definition
        if true_ty.is_none() {
            LogMesg::err()
                .name("Wrong member")
                .cause(format!("Member {} does not exist in {}", 
                               style(&memb_name).italic(), 
                               style(&struct_name).bold())
                       .as_str())
                .help(format!("Remove member {} from {} intiliazation",
                               style(&memb_name).italic(), 
                               style(&struct_name).bold())
                      .as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .lines(pair.as_str())
                .send()
                .unwrap();
        }

        let value = expr::parse_valued_expr(member.next().unwrap()); 
        let (value, ty) = check::node_type(value, true_ty.clone());

        let ty = match ty {
            Ok(t) => t,
            Err(e) => {
                e.location(pair.as_span().start_pos().line_col().0)
                 .lines(pair.as_str())
                 .send()
                 .unwrap();
                VarType::Unknown
            },
        };

        // check if the member type is correct 
        if let Some(tty) = true_ty {
            if let Err(e) = check::expect_type(tty.clone(), &ty) {
                e.location(pair.as_span().start_pos().line_col().0)
                 .lines(pair.as_str())
                 .send()
                 .unwrap();
            }
        }

        (memb_name, value)
    }).collect::<Vec<(String, AstNode)>>();

    // reoreder the members list following the true order of the members
    let mut members = vec![];
    if let Some(true_membs) = true_members {
        let mut missing_members = vec![];
        for (tm_name, _)in &true_membs  {
             match unordered_members.iter().position(|(name, _)| name == tm_name) {
                Some(index) => members.push(unordered_members[index].clone()),
                None => missing_members.push(tm_name.as_str()),
             }
        }

        if !missing_members.is_empty() {
            let missing_str = missing_members.iter()
                .map(|m| format!("{}", style(m).italic()))
                .collect::<Vec<String>>()
                .join(", ");

            LogMesg::err()            
                .name("Missing members")
                .cause(format!("Members of {} struct are missing", 
                               style(&struct_name).bold()).as_str())
                .help(format!("Consider adding the following members to struct initialization: {}", 
                              missing_str).as_str())
                .lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .send()
                .unwrap();
        }
    }

    AstNode::Strct { name: struct_name, members } 
}

pub fn parse_memb_access_expr(pair: Pair<Rule>) -> AstNode {
    let mut inner = pair.clone().into_inner();

    let parent_rule = inner.next().unwrap();
    let parent = match parent_rule.as_rule() {
        Rule::id => AstNode::Identifyer(parent_rule.as_str().into()),
        Rule::indexationExpr | Rule::unaryExpr | Rule::funCallExpr => expr::parse_valued_expr(parent_rule),
        _ => unreachable!(),
    };

    let (parent, parent_ty_res) = check::node_type(parent, None);
    let parent = Box::new(parent);

    let member_name = inner.next().unwrap().as_str();

    let check_member = |memb_name: &str, parent_ty_res: Result<VarType, LogMesg<String>>| {
        let (true_members, parent_ty) = match parent_ty_res {
            Ok(VarType::Struct(name)) => match ST.lock().unwrap().search_struct(&name) {
                Ok(tm) => (Some(tm), VarType::Struct(name)),
                Err(e) => {  // the struct does not exist in this scope
                    e.lines(pair.as_str())
                     .location(pair.as_span().start_pos().line_col().0)
                     .lines(pair.as_str())
                     .send()
                     .unwrap();

                    (None, VarType::Unknown)
                },
            },
            Ok(other_ty) => { // parent isn't a struct
                LogMesg::err()
                    .name("Illegal operation".to_string())
                    .cause(format!("Trying to access members of a non struct type: {:?}", other_ty))
                    .lines(pair.as_str())
                    .location(pair.as_span().start_pos().line_col().0)
                    .lines(pair.as_str())
                    .send()
                    .unwrap();

                (None, VarType::Unknown)
            },
            Err(e) => {     // there was an error obtaining the type of the parent node
                e.lines(pair.as_str())
                .location(pair.as_span().start_pos().line_col().0)
                .lines(pair.as_str())
                .send()
                .unwrap();

                (None, VarType::Unknown)
            },
        };

        let struct_name = match &parent_ty {
            VarType::Struct(n) => n,
            VarType::Unknown => return (0, VarType::Unknown, parent_ty),
            _ => unreachable!(),
        };

        let true_membs = match &true_members {
            Some(tm) => tm,
            None => return (0, VarType::Unknown, parent_ty),
        };

        // extract the "true" type of this member (extracted from the struct definition)
        match true_membs.iter().enumerate().find(|(_, (name, _))| *name == memb_name).map(|(i, (_, ty))| (i, ty.clone())) {
            Some((n, t)) => (n, t, parent_ty),
            None => {
                LogMesg::err()
                   .name("Wrong member")
                   .cause(format!("Member {} does not exist in {}", 
                                  style(&memb_name).italic(), 
                                  style(&struct_name).bold())
                          .as_str())
                   .location(pair.as_span().start_pos().line_col().0)
                   .lines(pair.as_str())
                   .send()
                   .unwrap();
                (0, VarType::Unknown, parent_ty)
            },
        }
    };

    let (member, member_ty, parent_ty) = check_member(member_name, parent_ty_res);

    let parent = AstNode::MemberAccessExpr { parent, member, member_ty, parent_ty };
    let (mut node, mut parent_ty_res) = check::node_type(parent, None);

    for rule in inner {
        let member_name = rule.as_str();
        let (member, member_ty, parent_ty) = check_member(&member_name, parent_ty_res);
        node = AstNode::MemberAccessExpr {
            parent: Box::new(node),
            member_ty,
            parent_ty,
            member,
        }; 
        let res = check::node_type(node, None);
        node = res.0;
        parent_ty_res = res.1;
    }
    
    node
}
