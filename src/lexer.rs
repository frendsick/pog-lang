use regex::{Captures,Match,Regex};
use crate::defs::{TOKEN_REGEXES,Token,TokenType};

#[derive(Debug)]
pub(crate) struct Parser<'a> {
  tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
  pub(crate) fn init(code: &'a str) -> Parser<'a> {
    Parser {
      tokenizer: Tokenizer::init(code),
    }
  }
  pub(crate) fn parse(&mut self) {
    loop {
      let token: Option<Token> = self.tokenizer.get_next_token();
      if token.is_none() { break }
      dbg!(&token);
    }
  }
}

#[derive(Debug)]
struct Tokenizer<'a> {
  code: &'a str,
  cursor: usize,
}

impl<'a> Tokenizer<'a> {
  fn init(code: &str) -> Tokenizer {
    Tokenizer { code: code, cursor: 0 }
  }

  fn has_more_tokens(&self) -> bool {
    self.cursor < self.code.len()
  }

  fn get_next_token(&mut self) -> Option<Token<'a>> {
    if !self.has_more_tokens() { return None }
    
    // Test if the remaining code matches with any Token regex
    let unparsed_code: &str = self.code.split_at(self.cursor).1;
    for (regex, token_type) in TOKEN_REGEXES.entries() {
      let captures: Option<Captures> = Regex::new(regex).unwrap().captures(unparsed_code);
      if !captures.is_none() {
        // Move cursor to the end of the parsed Token
        self.cursor += captures.as_ref().unwrap().get(0).unwrap().end();

        // Token should be skipped, e.g. whitespace or comment
        if token_type == &TokenType::None { return self.get_next_token() }

        // Take match from capture group if it is explicitly specified
        let mut matches: Option<Match> = captures.as_ref().unwrap().get(1);
        if matches.is_none() {
          matches = captures.unwrap().get(0);
        }

        return Some(Token {
          typ: token_type,
          value: matches.unwrap().as_str()
        })
      }
    }

    // TODO: Enhance error reporting
    panic!("Unknown Token at the start of the following code:\n{}", unparsed_code)
  }
}
