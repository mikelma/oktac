use pest::iterators::Pair;

use super::{parser::*, *};

pub fn parse_visibility(pair: Pair<Rule>) -> Visibility {
    match pair.as_str() {
        "pub" => Visibility::Pub,
        _ => unreachable!(),
    }
}
