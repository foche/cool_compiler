(* coolparser.mly *)

%{
open Util
module Abssyn = Abstractsyntax

let create_var_decl ~id ~typ =
  (Tables.make_id id, Tables.make_type typ)

let self_recv ~loc =
  Ast.create_expr ~expr:(Abssyn.Variable Tables.self_var) loc
%}

(* tokens *)

%token CLASS
%token INHERITS
%token ASSIGN
%token IF
%token THEN
%token ELSE
%token FI
%token WHILE
%token LOOP
%token POOL
%token CASE
%token OF
%token DARROW
%token ESAC
%token LET
%token IN
%token NEW
%token PLUS
%token MINUS
%token MULT
%token DIV
%token LT
%token LE
%token EQ
%token ISVOID
%token NEG
%token NOT
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token DOT
%token COMMA
%token SEMI
%token COLON
%token AT
%token EOF

%token <string> ERR
%token <string> TYPEID
%token <string> OBJECTID
%token <string> STR_CONST
%token <string> INT_CONST
%token <bool> BOOL_CONST

(* precedences *)

%nonassoc let_prec
%nonassoc ASSIGN
%nonassoc NOT
%nonassoc LE LT EQ
%left PLUS MINUS
%left MULT DIV
%nonassoc ISVOID
%nonassoc NEG
%nonassoc AT
%nonassoc DOT

(* start nonterminal *)

%start parse
%type <Abstractsyntax.class_list option * bool> parse

%%

(* productions *)

let parse :=
| cls = class_def+; EOF;
    { (Some cls, false) }
| EOF;
    { (None, true) }

let class_def :=
| CLASS; typ = TYPEID; ~ = parent; LBRACE; cl_features = feature*; RBRACE; SEMI;
    {
        {
            Abssyn.elem = {
                Abssyn.cl_typ = Tables.make_type typ;
                cl_parent = parent;
                cl_features;
            };
            loc = $loc;
        }
    }

let parent :=
| { Tables.object_type }
| INHERITS; typ = TYPEID;
    { Tables.make_type typ }

let feature :=
| id = OBJECTID; COLON; typ = TYPEID; ~ = init; SEMI;
    {
        {
            Abssyn.elem = Abssyn.Field (create_var_decl ~id ~typ, init);
            loc = $loc;
        }
    }
| id = OBJECTID; ~ = formals; COLON; ret_typ = TYPEID; LBRACE; method_body = expr; RBRACE; SEMI;
    {
        {
            Abssyn.elem = Abssyn.Method {
                method_id = Tables.make_id id;
                method_formals = formals;
                method_ret_typ = Tables.make_type ret_typ;
                method_body;
            };
            loc = $loc;
        }
    }

let init :=
| { Ast.no_expr ~loc:$loc }
| ASSIGN; expr

let formals :=
| LPAREN; ~ = separated_list(COMMA, formal); RPAREN;
    <>

let formal :=
| id = OBJECTID; COLON; typ = TYPEID;
    {
        {
            Abssyn.elem = create_var_decl ~id ~typ;
            loc = $loc;
        }
    }

let expr :=
| id = OBJECTID; ASSIGN; ~ = expr;
    {
        Ast.create_expr ~expr:(Abssyn.Assign (Tables.make_id id, expr))
        $loc
    }
| method_id = OBJECTID; dyn_args = args;
    {
        Ast.create_expr ~expr:(Abssyn.DynamicDispatch {
            dyn_recv = self_recv ~loc:$loc(method_id);
            dyn_method_id = Tables.make_id method_id;
            dyn_args;
        })
        $loc
    }
| dyn_recv = expr; DOT; method_id = OBJECTID; dyn_args = args;
    {
        Ast.create_expr ~expr:(Abssyn.DynamicDispatch {
            dyn_recv;
            dyn_method_id = Tables.make_id method_id;
            dyn_args;
        })
        $loc
    }
| stat_recv = expr; AT; target_typ = TYPEID; DOT; method_id = OBJECTID; ~ = args;
    {
        Ast.create_expr ~expr:(Abssyn.StaticDispatch {
            stat_recv;
            stat_target_typ = Tables.make_type target_typ;
            stat_method_id = Tables.make_id method_id;
            stat_args = args;
            stat_label = None;
        })
        $loc
    }
| IF; cond_pred = expr; THEN; cond_true = expr; ELSE; cond_false = expr; FI;
    { Ast.create_expr ~expr:(Abssyn.Cond {cond_pred; cond_true; cond_false}) $loc }
| WHILE; loop_pred = expr; LOOP; loop_body = expr; POOL;
    { Ast.create_expr ~expr:(Abssyn.Loop {loop_pred; loop_body}) $loc }
| LBRACE; block = terminated(expr, SEMI)+; RBRACE;
    { Ast.create_expr ~expr:(Abssyn.Block block) $loc }
| LET; bindings = rev(separated_nonempty_list(COMMA, binding)); IN; body = expr;
    { Ast.create_let ~bindings ~body } %prec let_prec
| CASE; case_expr = expr; OF; case_branches = branch+; ESAC;
    { Ast.create_expr ~expr:(Abssyn.Case {case_expr; case_branches}) $loc }
| NEW; typ = TYPEID;
    { Ast.create_expr ~expr:(Abssyn.New (Tables.make_type typ)) $loc }
| arith_e1 = expr; ~ = arith_op; arith_e2 = expr;
    {
        Ast.create_expr ~expr:(Abssyn.Arith {arith_op; arith_e1; arith_e2})
        $loc
    }
| comp_e1 = expr; ~ = comp_op; comp_e2 = expr;
    { Ast.create_expr ~expr:(Abssyn.Comp {comp_op; comp_e1; comp_e2}) $loc }
| e1 = expr; EQ; e2 = expr;
    { Ast.create_expr ~expr:(Abssyn.Eq (e1, e2)) $loc }
| op = unary; e = expr;
    { Ast.create_expr ~expr:(op e) $loc }
| LPAREN; ~ = expr; RPAREN;
    <>
| id = OBJECTID;
    { Ast.create_expr ~expr:(Abssyn.Variable (Tables.make_id id)) $loc }
| x = INT_CONST;
    { Ast.create_expr ~expr:(Abssyn.IntConst (Tables.make_int x)) $loc }
| s = STR_CONST;
    { Ast.create_expr ~expr:(Abssyn.StrConst (Tables.make_str s)) $loc }
| x = BOOL_CONST;
    { Ast.create_expr ~expr:(Abssyn.BoolConst x) $loc }

(* this needs to be a macro to avoid shift/reduce conflicts *)
let arith_op ==
| PLUS;
    { Abssyn.Plus }
| MINUS;
    { Abssyn.Minus }
| MULT;
    { Abssyn.Mult }
| DIV;
    { Abssyn.Div }

(* same as above *)
let comp_op ==
| LT;
    { Abssyn.Lt }
| LE;
    { Abssyn.Le }

(* same as above *)
let unary ==
| NEG;
    { (fun e -> Abssyn.Neg e) }
| NOT;
    { (fun e -> Abssyn.Not e) }
| ISVOID;
    { (fun e -> Abssyn.IsVoid e) }

let binding :=
| id = OBJECTID; COLON; typ = TYPEID; ~ = init;
    { (Tables.make_id id, Tables.make_type typ, init, $loc) }

let branch :=
| id = OBJECTID; COLON; typ = TYPEID; DARROW; body = expr; SEMI;
    {
        {
            Abssyn.elem = (create_var_decl ~id ~typ, body);
            loc = $loc;
        }
    }

let args := LPAREN; ~ = separated_list(COMMA, expr); RPAREN;
    <>
