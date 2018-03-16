use lexer::{Token, TokenType};
use var::*;
use var::SVar::*;
use std::vec::Vec;
use std::process;
use std::f64;

/*
 * tix:
 *  tokens[tix].tstr == "1st argument"
 *
 * returned tix:
 *   tokens[tix].tstr == ")"
 * */

fn func_get_arg(tokens: &Vec<Token>,
                vars: &mut Vec<Var>,
                tlen: usize,
                tix: &mut usize) -> Result<Vec<SVar>, String> {
    let mut res: Vec<SVar> = vec![];

    while (tokens[*tix].tstr != ")") && (*tix < tlen) {
        let value = try!(expr(tokens, vars, tlen, tix));
        res.push(value);

        if (*tix < tlen) && (tokens[*tix].tstr == ",") {
            *tix += 1;
        }
        else { // tokens[tix].tstr == ")"
            break;
        }
    }

    Ok(res)
}

fn array_proc(tokens: &Vec<Token>,
              vars: &mut Vec<Var>,
              tlen: usize,
              tix: &mut usize) -> Result<SVar, String> {
    let mut res: Vec<SVar> = vec![];

    while (tokens[*tix].tstr != "]") && (*tix < tlen) {
        let value = try!(expr(tokens, vars, tlen, tix));
        res.push(value);

        if tokens[*tix].tstr == "," {
            *tix += 1;
        }
        else { // tokens[tix].tstr == "]"
            break;
        }
    }

    Ok(SArr(res))
}

fn func_get_arg2(tokens: &Vec<Token>,
                tlen: usize,
                tix: &mut usize) -> Result<Vec<String>, String> {
    let mut res: Vec<String> = vec![];

    while (tokens[*tix].tstr != ")") && (*tix < tlen) {
        let value = tokens[*tix].tstr.clone();
        res.push(value);
        *tix += 1;

        if tokens[*tix].tstr == "," {
            *tix += 1;
        }
        else { // tokens[tix].tstr == ")"
            break;
        }
    }

    Ok(res)
}
 

fn func_call(tokens: &Vec<Token>,
             vars: &mut Vec<Var>,
             tlen: usize,
             tix: &mut usize,
             x: &Var) -> Result<SVar, String> {
    if (*x).vtype == VarType::BuiltInFunc {
        *tix += 2;

        let mut result = Ok(SNum(f64::NAN));
        let funcname = (*x).vname.clone();
        let __args = func_get_arg(tokens, vars, tlen, tix);

        // 
        match funcname.as_str() {
            "exit" => process::exit(0),
            "quit" => process::exit(0),
            _      => (),
        };

        // num
        let _args = try!(__args);
        let args = rem_snum((_args.clone()));
        let lenargs = args.len();
        let mut fooo = 0;


        // 1 arg
        result =
        match funcname.as_str() {
            "ln" => Ok(SNum(args[0].ln())),
            "sin" => Ok(SNum(args[0].sin())),
            "cos" => Ok(SNum(args[0].cos())),
            "tan" => Ok(SNum(args[0].tan())),
            "exp" => Ok(SNum(args[0].exp())),
            /*
            "log" => Ok(SNum(args[0].log(args[1]))),
            "pow" => Ok(SNum(args[0].powf(args[1]))),
            "max" => Ok(SNum(args[0].max(args[1]))),
            "min" => Ok(SNum(args[0].min(args[1]))),
            */
            "asin" => Ok(SNum(args[0].asin())),
            "acos" => Ok(SNum(args[0].acos())),
            "atan" => Ok(SNum(args[0].atan())),
            "sqrt" => Ok(SNum(args[0].sqrt())),
            "cbrt" => Ok(SNum(args[0].cbrt())),
            "arcsin" => Ok(SNum(args[0].asin())),
            "arccos" => Ok(SNum(args[0].acos())),
            "arctan" => Ok(SNum(args[0].atan())),
            "is_matrix" => Ok(SBool(is_matrix(&_args[0]))),
            "to_degrees" => Ok(SNum(args[0].to_degrees())),
            "to_radians" => Ok(SNum(args[0].to_radians())),



            "puts" => {
                let pp = try!(unpack_str(_args[0].clone()));
                print!("{}", pp);
                Ok(SNum(0.0 as f64))
            },


            "print" => {
                let pp = try!(unpack_str(_args[0].clone()));
                print!("{}", pp);
                Ok(SNum(0.0 as f64))
            },
            
            "println" => {
                let pp = try!(unpack_str(_args[0].clone()));
                println!("{}", pp);
                Ok(SNum(0.0 as f64))
            },

            _ =>
            {
                fooo = 1;
                Ok(SNum(f64::NAN))
            }, // Err("no function named ".to_owned() + &tokens[*tix].tstr),
        };


        // when argments are not enough
        if (fooo == 1) && (lenargs < 2) {
            return Err("Arguments are not enough".to_owned());
        }
        else if fooo != 1 { return result; }


        // 2 args
        result =
        match funcname.as_str() {
            "log" => Ok(SNum(args[0].log(args[1]))),
            "pow" => Ok(SNum(args[0].powf(args[1]))),
            "max" => Ok(SNum(args[0].max(args[1]))),
            "min" => Ok(SNum(args[0].min(args[1]))),
            _ => result,
        };


        return result;
                        
    } else if (*x).vtype == VarType::UserFunc {
        // Ok(x.vval)
        *tix += 2;

        let args = try!(func_get_arg(tokens, vars, tlen, tix));
        
        let jjj = match (*x).vval.clone() {
            SUFunc(y) => Ok(y),
            _         => Err("what is the error message?"),
        }; //sufunc
        let jj = try!(jjj);
        let funcargs = &(jj.ufargs);
        let ttt = jj.ufval.clone();
        let tttlen = ttt.len();
        let mut cpvars = (*vars).clone();

        for i in 0..(funcargs.len()) {

            let t = get_svar_type(&args[i]);

            cpvars.push(Var{vname: (*funcargs)[i].clone(),
                            vtype: t,
                            vval: args[i].clone() });
        }

        expr( &ttt, &mut cpvars, tttlen, &mut 0 )
    } else {
        Ok((*x).vval.clone())
    }
}

fn getvar(tokens: &Vec<Token>,
          vars: &mut Vec<Var>,
          tlen: usize,
          tix: &mut usize) -> Result<SVar, String> {

    if (tlen - *tix) < 1 { return Err("at function getvar.".to_string()); }

    let t: Result<SVar, String>;

    if tokens[*tix].tstr == "(" {
        *tix += 1;
        t = expr(tokens, vars, tlen, tix);
    }

    else if tokens[*tix].tstr == "{" {
        let t_len = vars.len();
        *tix += 1;
        let mut tc = tokens.clone();
        let mut vc = vars.clone();
        t = parser_2(&mut tc, &mut vc, tlen, tix);
        vc.truncate(t_len);
        // println!("{:?}", vc);
        *vars = vc;
    }

    else if tokens[*tix].tstr == "[" {
        *tix += 1;
        t = array_proc(tokens, vars, tlen, tix);
    }

    else if tokens[*tix].ttype == TokenType::DIGIT {
        t = Ok(SNum (try!(
                (&(tokens[*tix].tstr)).parse::<f64>()
                .map_err(|e| e.to_string())
                )));
    }

    else if tokens[*tix].tstr == "+" {
        *tix += 1;
        t = Ok(try!(getvar(tokens, vars, tlen, tix)));
    }

    else if tokens[*tix].tstr == "-" {
        *tix += 1;
        t = Ok(SNum(-1.0f64) * try!(getvar(tokens, vars, tlen, tix)));

    }

    else if tokens[*tix].ttype == TokenType::STRING {
        t = Ok(SStr(tokens[*tix].tstr.clone()));
    }

    else {
        let q = vars.clone();
        let p = q.iter().find(|x| x.vname == tokens[*tix].tstr);
        t = match p {
                None => Err("no variable named ".to_owned() + &tokens[*tix].tstr),
                Some(x) => func_call(tokens, vars, tlen, tix, &x),
            }
    }

    *tix += 1;
    t
}

fn bot(tokens: &Vec<Token>,
       vars: &mut Vec<Var>,
       tlen: usize,
       tix: &mut usize) -> Result<SVar, String> {
    let mut v1 = try!(getvar(tokens, vars, tlen, tix));

    while (*tix < tlen) && (tokens[*tix].tstr == "^" || tokens[*tix].tstr == ")") {
        let ope = &tokens[*tix].tstr;
        if ope == ")" { break; }

        *tix += 1;
        let jjj = match v1 {
            SNum(x) => Ok(x),
            _       => Err("I don't frickin know what this error is."),
        }; //sufunc
        let v13 = try!(jjj);
        let v2 = try!(getvar(tokens, vars, tlen, tix));
        let jjj = match v2 {
            SNum(x) => Ok(x),
            _       => Err("what is the error message?"),
        }; //sufunc
        let v23 = try!(jjj);
        v1 = SNum(v13.powf(v23));
    }

    Ok(v1)

}

fn mul(tokens: &Vec<Token>,
       vars: &mut Vec<Var>,
       tlen: usize,
       tix: &mut usize) -> Result<SVar, String> {
    let mut v1 = try!(bot(tokens, vars, tlen, tix));

    while (*tix < tlen) && (tokens[*tix].tstr == "*" || tokens[*tix].tstr == "/") {
        let ope = &tokens[*tix].tstr;
        // if ope == ")" { break; }

        *tix += 1;
        let v2 = try!(bot(tokens, vars, tlen, tix));

        if ope == "*" {
            v1 = v1 * v2;
        }
        else if ope == "/"{
            v1 = v1 / v2;
        }
    }

    Ok(v1)
}

fn expr(tokens: &Vec<Token>,
        vars: &mut Vec<Var>,
        tlen: usize,
        tix: &mut usize) -> Result<SVar, String> {
    let mut v1 = try!(mul(tokens, vars, tlen, tix));

    while (*tix < tlen) && (tokens[*tix].tstr == "+" || tokens[*tix].tstr == "-") {
        let ope = &tokens[*tix].tstr;
        // if ope == ")" { break; }

        *tix += 1;
        let v2 = try!(mul(tokens, vars, tlen, tix));

        if ope == "+" {
            v1 = v1 + v2;
        }
        else {
            v1 = v1 - v2;
        }
    }

    Ok(v1)
}

fn eqex(tokens: &Vec<Token>,
        vars: &mut Vec<Var>,
        tlen: usize,
        tix: &mut usize) -> Result<SVar, String> {
    let mut v1 = try!(expr(tokens, vars, tlen, tix));
    let mut res = v1.clone();

    while (*tix < tlen) && (tokens[*tix].tstr == "==" || tokens[*tix].tstr == "!=") {
        let ope = &tokens[*tix].tstr;

        *tix += 1;
        let v2 = try!(expr(tokens, vars, tlen, tix));

        if ope == "==" {
            res = SBool(v1 == v2);
        }
        else if ope == "!=" {
            res = SBool(v1 != v2);
        }

        v1 = v2;
    }

    Ok(res)
}

fn parser_2(tokens: &mut Vec<Token>,
            vars: &mut Vec<Var>,
            tlen: usize,
            tix: &mut usize) -> Result<SVar, String> {
    let retv: Result<SVar, String>;

    if tlen < 1 {
        return Ok(SNum(0.0 as f64));
    }

    if tlen > (*tix + 1) && (tokens[*tix + 1].tstr == "=") { // define a variable
        let mut foo = true;

        let vn = tokens[*tix].tstr.clone();
        *tix += 2;
        let res = try!(parser_2(tokens, vars, tlen, tix));
        let vrtype = get_svar_type(&res);

        for i in 0..vars.len() {
            if vn != vars[i].vname { continue; }
            foo = false;
            vars[i].vval = res.clone();
            vars[i].vtype = vrtype.clone();
        }

        if foo {
            vars.push(Var{vname: vn, vtype: vrtype, vval: res.clone()})
        }

        retv = Ok(res);
    }
    else if tokens[*tix].tstr == "defun" { // define a function
        *tix += 1;
        let fun = tokens[*tix].tstr.clone();
        *tix += 2;
        let args = try!(func_get_arg2(tokens, tlen, tix));
        *tix += 1;

        // err!
        if tokens[*tix].tstr != "=" {
            return Err("Error at defining function".to_owned());
        }

        *tix += 1;
        

        let mut res = tokens.clone();
        res.reverse();
        res.truncate(tlen - *tix);
        res.reverse();

        let function = UFunc { ufname: fun.clone(), ufargs: args, ufval: res };
        vars.push(Var{vname: fun.clone(), vtype: VarType::UserFunc, vval: SUFunc(function) });

        retv = Ok(SNum(0.0 as f64));
    }
    else {
        retv = eqex(tokens, vars, tlen, tix);
    }

    return retv;
}

fn parser_2_1(tokens: &mut Vec<Token>,
            vars: &mut Vec<Var>,
            tlen: usize,
            tix: &mut usize) -> Result<SVar, String> {
    let mut retv: Result<SVar, String>;

    if tlen < 1 {
        return Ok(SNum(0.0 as f64));
    }

    retv = parser_2(tokens, vars, tlen, tix);

    while (*tix + 1) < tlen && (tokens[*tix].tstr == ";" || tokens[*tix].tstr == "}") {
        *tix += 1;
        let p = *tix;

        retv = parser_2_1(tokens, vars, tlen, tix);

        if tokens[p - 1].tstr == "}" || tokens[p].tstr == "}" { println!("dd={}\n", tokens[p-1].tstr); break; }
    }

   
    return retv;
}

pub fn parser(tokens: &mut Vec<Token>,
              vars: &mut Vec<Var>) -> Result<SVar, String> {
    let l = tokens.len();
    parser_2_1(tokens, vars, l, &mut 0)
}

