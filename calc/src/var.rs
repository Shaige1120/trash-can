use lexer::{Token};
use std::f64;
use std::ops::{Add, Sub, Mul, Div};
use std::fmt;
use std::cmp::max;


#[derive(PartialEq, Clone, Debug)]
pub enum VarType {
    Digits,
    Str,
    Arr,
    Bool,
    UserFunc,
    BuiltInFunc,
}

#[derive(PartialEq, Clone, Debug)]
pub struct UFunc { // User function
    pub ufname: String, // name
    pub ufargs: Vec<String>, // varriable names of args
    pub ufval: Vec<Token>, // tokens
}

#[derive(PartialEq, Clone, Debug)]
pub struct BFunc1 { // built-in function
    pub bfname: &'static str, // name
}


#[derive(PartialEq, Clone, Debug)]
pub enum SVar {
    SNum(f64),       // VarType::Digits
    SArr(Vec<SVar>), //        ::Arr
    SStr(String),    //        ::Str
    SBool(bool),     //        ::Bool
    SBFunc(String),  //        ::BuiltInFunc
    SUFunc(UFunc),   //        ::UserFunc
}

#[derive(PartialEq, Clone, Debug)]
pub struct Var {
    pub vname: String,
    pub vtype: VarType,
    pub vval: SVar,
    // pub vfun: BFunc1,
    // pub ufun: UFunc,
}


impl Add for SVar {
    type Output = SVar;

    fn add(self, oth: SVar) -> SVar {
        match self {
            SVar::SNum(x) =>
            {
                let ss = match oth {
                    SVar::SNum(y) => y,
                    _             => 0.0 as f64,
                };
                SVar::SNum(x + ss)
            },

            SVar::SArr(x) =>
            {
                let y = match oth {
                    SVar::SArr(ss) => ss,
                    _              => x.clone(),
                };

                let mut r = vec![];
                let xl = x.len();
                let yl = y.len();
                let l =  max(xl, yl);// xl.max(yl);
                

                for i in 0..l {
                    if xl <= i {
                        r.push(y[i].clone());
                    }
                    else  if yl <= i{ 
                        r.push(x[i].clone());
                    }       
                    else {
                        r.push(x[i].clone() + y[i].clone());
                    }
                }

                SVar::SArr(r)
                
            },

            _ => self,
        }
    }
}

impl Sub for SVar {
    type Output = SVar;

    fn sub(self, oth: SVar) -> SVar {
        match self {
            SVar::SNum(x) =>
            {
                let ss = match oth{
                    SVar::SNum(y) => y,
                    _             => 0.0,
                };
                SVar::SNum(x - ss)
            },

            SVar::SArr(x) =>
            {
                let y = match oth {
                    SVar::SArr(ss) => ss,
                    _              => x.clone(),
                };

                let mut r = vec![];
                let xl = x.len();
                let yl = y.len();
                let l  = max(xl,yl); // xl.max(yl);
                

                for i in 0..l {
                    if xl <= i {
                        r.push(y[i].clone());
                    }
                    else  if yl <= i{ 
                        r.push(x[i].clone());
                    }       
                    else {
                        r.push(x[i].clone() - y[i].clone());
                    }
                }

                SVar::SArr(r)
                
            },

            _ => self
        }
    }
}


impl Mul for SVar {
    type Output = SVar;

    fn mul(self, oth: SVar) -> SVar {
        match self {
            SVar::SNum(x) =>
            {
                let ss = match oth{
                    SVar::SNum(y) => y,
                    _             => 0.0,
                };
                SVar::SNum(x * ss)
            },

            _ => self
        }
    }
}

impl Div for SVar {
    type Output = SVar;

    fn div(self, oth: SVar) -> SVar {
        match self {
            SVar::SNum(x) =>
            {
                let ss = match oth{
                    SVar::SNum(y) => y,
                    _             => 0.0,
                };
                SVar::SNum(x / ss)
            },

            _ => self
        }
    }
}

impl fmt::Display for SVar {

    #[allow(unused_must_use)]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (*self).clone() {
            SVar::SNum(x) => write!(f, "{}", x),
            SVar::SStr(x) => write!(f, "{}", x),
            SVar::SArr(x) =>
            {
                let l = x.len();
                
                write!(f, "[{}", x[0]);
                for i in 1..l {
                    write!(f, ", ");
                    write!(f, "{}", x[i]);
                }
                write!(f, "]")
            },
            SVar::SBFunc(x) => write!(f, "{:?}", x),
            SVar::SUFunc(x) => write!(f, "{:?}", x),
            SVar::SBool(x)  => write!(f, "{}", x),
        }

        
    }
}

pub fn unpack_str(s: SVar) -> Result<String, String> {
    match s {
        SVar::SStr(le) => Ok(le),
        _        => Err("Not a string".to_owned()),
    }
}

pub fn rem_snum(vs: Vec<SVar>) -> Vec<f64> {
    let mut res: Vec<f64> = vec![];
    let l = vs.len();

    for i in 0..l {
        match vs[i] {
            SVar::SNum(j) => {
                res.push(j);
            },

            _ => (),
        };
        //let SVar::SNum(j) = vs[i];
        //res.push(j);
    }

    res
}

pub fn is_matrix(v: &SVar) -> bool {
    match (*v).clone() {
        SVar::SArr(a) =>
        {
            let l = a.len();
            let row = match a[0].clone() {
                SVar::SArr(b) => b.len(),
                _             => return false,
            };

            for i in 1..l {
                match a[i].clone() {
                    SVar::SArr(b) =>
                    {
                        let newrow = b.len();
                        if newrow != row { return false; }
                    },
                    _             => return false,
                };
            }

            return true;
        },
        _             => false,
    }
}

pub fn get_svar_type(x: &SVar) -> VarType {
    match *x {
        SVar::SNum(_) => VarType::Digits,
        SVar::SArr(_) => VarType::Arr,
        SVar::SStr(_) => VarType::Str,
        SVar::SBool(_) => VarType::Bool,
        SVar::SBFunc(_) => VarType::BuiltInFunc,
        SVar::SUFunc(_) => VarType::UserFunc,
    }
}

// pub fn dummy_func_bfunc1(x: f64) -> f64 {x}
// pub const DUMMY_BFUNC1: BFunc1 = BFunc1 {bfname: ""};

fn ret_builtin_func(vn: &'static str) -> Var {Var{vname: vn.to_owned(), vtype: VarType::BuiltInFunc, vval: SVar::SBFunc(vn.to_string())}}

pub fn init_var(vars: &mut Vec<Var>) {
    vars.push(Var {vname: "pi".to_string() ,   vtype: VarType::Digits, vval: SVar::SNum(f64::consts::PI)});
    vars.push(Var {vname: "true".to_string(),  vtype: VarType::Bool,   vval: SVar::SBool(true)});
    vars.push(Var {vname: "false".to_string(), vtype: VarType::Bool,   vval: SVar::SBool(false)});


    vars.push(ret_builtin_func("sin"));
    vars.push(ret_builtin_func("cos"));
    vars.push(ret_builtin_func("tan"));
    vars.push(ret_builtin_func("exp"));
    vars.push(ret_builtin_func("log"));
    vars.push(ret_builtin_func("ln"));
    vars.push(ret_builtin_func("asin"));
    vars.push(ret_builtin_func("acos"));
    vars.push(ret_builtin_func("atan"));
    vars.push(ret_builtin_func("max"));
    vars.push(ret_builtin_func("min"));
    vars.push(ret_builtin_func("is_matrix"));
    vars.push(ret_builtin_func("to_degrees"));
    vars.push(ret_builtin_func("to_radians"));



    vars.push(ret_builtin_func("puts"));
    vars.push(ret_builtin_func("print"));
    vars.push(ret_builtin_func("println"));
    vars.push(ret_builtin_func("exit"));
    vars.push(ret_builtin_func("quit"));

}
