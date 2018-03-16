// use std::vec::Vec;
use std::str;
// use std::option::Option;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    CHAR,
    DIGIT,
    STRING,
    SYMBOL,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub tstr : String,
    pub ttype :TokenType,
}

pub fn lexer(srcstr: &str) -> Vec<Token> {
    let mut vec    : Vec<Token> = Vec::new();
    let mut src                 = srcstr.chars();
    let mut nxt                 = src.next();
    let mut c      : char;

    while nxt.is_some() {
        c = nxt.unwrap();

        let mut tmp : String = String::new();
        let     tmptype : TokenType;

        if c == ' ' || c == '\n' {
            nxt = src.next();
            continue;
        }

        if c.is_alphabetic() {
            tmp.push(c);
            tmptype = TokenType::CHAR;
            nxt = src.next();
    
            if nxt.is_some() {
                c = nxt.unwrap();

                while c.is_alphabetic() || c.is_digit(10) || c == '_' {
                    tmp.push(c);
                    nxt = src.next();
                    if nxt.is_some() { c = nxt.unwrap(); } else { break; }
                }
            }
        }
        else if c.is_digit(10) {
            tmp.push(c);
            tmptype = TokenType::DIGIT;
            nxt = src.next();

            if nxt.is_some() {
                c = nxt.unwrap();

                while c.is_digit(10) || c == '.' {
                    tmp.push(c);
                    nxt = src.next();
                    if nxt.is_some() { c = nxt.unwrap(); } else { break; }
                }
            }
        }
        else if c == '"'{
            tmptype = TokenType::STRING;
            nxt = src.next();

            if nxt.is_some() {
                c = nxt.unwrap();

                while c != '"' {
                    tmp.push(c);
                    nxt = src.next();
                    if nxt.is_some() {c = nxt.unwrap();} else {break;}
                }
                if nxt.is_some() {nxt = src.next();}
            }
        }
        else {
            tmp.push(c);
            tmptype = TokenType::SYMBOL;
            nxt = src.next();

            if nxt.is_some() {
                let d = nxt.unwrap();

                // !=, ==
                if (c == '=' || c == '!') && (d == '=') {
                    tmp.push(d);
                    nxt = src.next();
                }
            }
        }

        vec.push(Token {tstr: tmp, ttype: tmptype});
    }

    vec
} 
