(* coolparser.mly *)

%{
module Abssyn = Abstractsyntax
module Tbls = Util.Tables
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

%token <String.t> ERR
%token <String.t> TYPEID
%token <String.t> OBJECTID
%token <String.t> STR_CONST
%token <String.t> INT_CONST
%token <Bool.t> BOOL_CONST

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
%type <Abstractsyntax.class_list Option.t * Bool.t> parse

%%

(* productions *)

let parse :=
| cls = class_def+; EOF;
    { (Some cls, false) }
| EOF;
    { (None, true) }

let class_def :=
| CLASS; typ = TYPEID; cl_parent = parent; LBRACE; cl_features = feature*; RBRACE; SEMI;
    {
        Ast.create_node ~loc:$loc
            {
                Abssyn.cl_typ = Tbls.make_type typ;
                cl_parent;
                cl_features;
            }
    }

let parent :=
| { Tbls.object_type }
| INHERITS; typ = TYPEID;
    { Tbls.make_type typ }

let feature :=
| field_var = var_decl; field_init = init; SEMI;
    { Abssyn.Field { Abssyn.field_var; field_init } |> Ast.create_node ~loc:$loc }
| id = OBJECTID; ~ = formals; COLON; ret_typ = TYPEID; LBRACE; ~ = expr; RBRACE; SEMI;
    {
        Abssyn.Method {
            Abssyn.method_id = Tbls.make_id id;
            method_formals = formals;
            method_ret_typ = Tbls.make_type ret_typ;
            method_body = expr;
        }
        |> Ast.create_node ~loc:$loc
    }

let init :=
| { Ast.no_expr ~loc:$loc }
| ASSIGN; expr

let formals :=
| LPAREN; ~ = separated_list(COMMA, var_decl); RPAREN;
    <>

let var_decl :=
| id = OBJECTID; COLON; typ = TYPEID;
    { Ast.create_var_node ~id ~typ ~loc:$loc }

let expr :=
| id = OBJECTID; ASSIGN; ~ = expr;
    { Ast.create_expr ~loc:$loc (Abssyn.Assign (Tbls.make_id id, expr)) }
| method_id = OBJECTID; dyn_args = args;
    {
        Abssyn.DynamicDispatch {
            Abssyn.dyn_recv = Ast.self_var_expr ~loc:$loc(method_id);
            dyn_method_id = Tbls.make_id method_id;
            dyn_args;
        }
        |> Ast.create_expr ~loc:$loc
    }
| dyn_recv = expr; DOT; method_id = OBJECTID; dyn_args = args;
    {
        Abssyn.DynamicDispatch {
            Abssyn.dyn_recv;
            dyn_method_id = Tbls.make_id method_id;
            dyn_args;
        }
        |> Ast.create_expr ~loc:$loc
    }
| stat_recv = expr; AT; target_typ = TYPEID; DOT; method_id = OBJECTID; stat_args = args;
    {
        Abssyn.StaticDispatch {
            Abssyn.stat_recv;
            stat_target_typ = Tbls.make_type target_typ;
            stat_method_id = Tbls.make_id method_id;
            stat_args;
            stat_label = None;
        }
        |> Ast.create_expr ~loc:$loc
    }
| IF; cond_pred = expr; THEN; cond_true = expr; ELSE; cond_false = expr; FI;
    { Abssyn.Cond { Abssyn.cond_pred; cond_true; cond_false } |> Ast.create_expr ~loc:$loc }
| WHILE; loop_pred = expr; LOOP; loop_body = expr; POOL;
    { Abssyn.Loop { Abssyn.loop_pred; loop_body } |> Ast.create_expr ~loc:$loc }
| LBRACE; exprs = rev(terminated(expr, SEMI)+); RBRACE;
    { Abssyn.Block (List.tl exprs |> List.rev, List.hd exprs) |> Ast.create_expr ~loc:$loc }
| LET; bindings = rev(separated_nonempty_list(COMMA, binding)); IN; body = expr;
    { Ast.create_let ~bindings ~body } %prec let_prec
| CASE; case_expr = expr; OF; case_branches = branch+; ESAC;
    { Abssyn.Case { Abssyn.case_expr; case_branches } |> Ast.create_expr ~loc:$loc }
| NEW; typ = TYPEID;
    { Abssyn.New (Tbls.make_type typ) |> Ast.create_expr ~loc:$loc }
| arith_e1 = expr; ~ = arith_op; arith_e2 = expr;
    { Abssyn.Arith { Abssyn.arith_op; arith_e1; arith_e2 } |> Ast.create_expr ~loc:$loc }
| comp_e1 = expr; ~ = comp_op; comp_e2 = expr;
    { Abssyn.Comp { Abssyn.comp_op; comp_e1; comp_e2 } |> Ast.create_expr ~loc:$loc }
| e1 = expr; EQ; e2 = expr;
    { Abssyn.Eq (e1, e2) |> Ast.create_expr ~loc:$loc }
| op = unary_op; e = expr;
    { op e |> Ast.create_expr ~loc:$loc }
| LPAREN; ~ = expr; RPAREN;
    <>
| id = OBJECTID;
    { Abssyn.Variable (Tbls.make_id id) |> Ast.create_expr ~loc:$loc }
| x = INT_CONST;
    { Abssyn.IntConst (Tbls.make_int x) |> Ast.create_expr ~loc:$loc }
| s = STR_CONST;
    { Abssyn.StrConst (Tbls.make_str s) |> Ast.create_expr ~loc:$loc }
| x = BOOL_CONST;
    { Abssyn.BoolConst x |> Ast.create_expr ~loc:$loc }

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
let unary_op ==
| NEG;
    { fun e -> Abssyn.Neg e }
| NOT;
    { fun e -> Abssyn.Not e }
| ISVOID;
    { fun e -> Abssyn.IsVoid e }

let binding :=
| ~ = var_decl; ~ = init;
    { (var_decl, init, $loc) }

let branch :=
| branch_var = var_decl; DARROW; branch_body = expr; SEMI;
    { Ast.create_node ~loc:$loc { Abssyn.branch_var; branch_body } }

let args :=
| LPAREN; ~ = separated_list(COMMA, expr); RPAREN;
    <>
