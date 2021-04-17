(* coolparser.mly *)

%{
open Util
module Abssyn = Abstractsyntax

let create_var_decl ~id ~typ =
  (Tables.make_id id, Tables.make_type typ)

let self_recv ~startpos ~endpos =
  Ast.create_expr ~expr:(Abssyn.Variable Tables.self_var) startpos endpos
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
| CLASS; typ = TYPEID; ~ = parent; LBRACE; features = feature*; RBRACE; SEMI;
    { { Abssyn.elem= {Abssyn.typ= Tables.make_type typ; parent; features}
      ; startpos= $startpos
      ; endpos= $endpos } }

let parent :=
| { Tables.object_type }
| INHERITS; typ = TYPEID;
    { Tables.make_type typ }

let feature :=
| id = OBJECTID; COLON; typ = TYPEID; ~ = init; SEMI;
    { {Abssyn.elem= Abssyn.Field (create_var_decl ~id ~typ, init); startpos= $startpos; endpos= $endpos} }
| method_id = OBJECTID; ~ = formals; COLON; ret_type = TYPEID; LBRACE; body = expr; RBRACE; SEMI;
    { { Abssyn.elem= Abssyn.Method
        { method_id= Tables.make_id method_id
        ; formals
        ; ret_type= Tables.make_type ret_type
        ; body }
      ; startpos= $startpos
      ; endpos= $endpos } }

let init :=
| { Ast.no_expr ~startpos:$startpos ~endpos:$endpos }
| ASSIGN; expr

let formals :=
| LPAREN; ~ = separated_list(COMMA, formal); RPAREN;
    <>

let formal :=
| id = OBJECTID; COLON; typ = TYPEID;
    { {Abssyn.elem= create_var_decl ~id ~typ; startpos= $startpos; endpos= $endpos} }

let expr :=
| id = OBJECTID; ASSIGN; ~ = expr;
    { Ast.create_expr ~expr:(Abssyn.Assign (Tables.make_id id, expr))
      $startpos $endpos }
| method_id = OBJECTID; ~ = args;
    { Ast.create_expr ~expr:(Abssyn.DynamicDispatch
        { recv= self_recv ~startpos:$startpos ~endpos:$endpos(method_id)
        ; method_id= Tables.make_id method_id
        ; args })
      $startpos $endpos }
| recv = expr; DOT; method_id = OBJECTID; ~ = args;
    { Ast.create_expr ~expr:(Abssyn.DynamicDispatch
        { recv
        ; method_id= Tables.make_id method_id
        ; args })
      $startpos $endpos }
| recv = expr; AT; target = TYPEID; DOT; method_id = OBJECTID; ~ = args;
    { Ast.create_expr ~expr:(Abssyn.StaticDispatch
        { recv
        ; target= Tables.make_type target
        ; method_id= Tables.make_id method_id
        ; args
        ; label= None })
      $startpos $endpos }
| IF; pred = expr; THEN; true_branch = expr; ELSE; false_branch = expr; FI;
    { Ast.create_expr ~expr:(Abssyn.Cond {pred; true_branch; false_branch})
      $startpos $endpos }
| WHILE; pred = expr; LOOP; body = expr; POOL;
    { Ast.create_expr ~expr:(Abssyn.Loop {pred; body}) $startpos $endpos }
| LBRACE; block = terminated(expr, SEMI)+; RBRACE;
    { Ast.create_expr ~expr:(Abssyn.Block block) $startpos $endpos }
| LET; bindings = separated_nonempty_list(COMMA, binding); IN; body = expr; %prec let_prec
    { Ast.create_let ~bindings ~body }
| CASE; ~ = expr; OF; branches = branch+; ESAC;
    { Ast.create_expr ~expr:(Abssyn.Case {expr; branches}) $startpos $endpos }
| NEW; typ = TYPEID;
    { Ast.create_expr ~expr:(Abssyn.New (Tables.make_type typ)) $startpos $endpos }
| ISVOID; ~ = expr;
    { Ast.create_expr ~expr:(Abssyn.IsVoid expr) $startpos $endpos }
| e1 = expr; op = binop; e2 = expr;
    { Ast.create_expr ~expr:(op e1 e2) $startpos $endpos }
| e = expr; op = unary;
    { Ast.create_expr ~expr:(op e) $startpos $endpos }
| LPAREN; ~ = expr; RPAREN;
    <>
| id = OBJECTID;
    { Ast.create_expr ~expr:(Abssyn.Variable (Tables.make_id id)) $startpos $endpos }
| x = INT_CONST;
    { Ast.create_expr ~expr:(Abssyn.IntConst (Tables.make_int x)) $startpos $endpos }
| s = STR_CONST;
    { Ast.create_expr ~expr:(Abssyn.StrConst (Tables.make_str s)) $startpos $endpos }
| x = BOOL_CONST;
    { Ast.create_expr ~expr:(Abssyn.BoolConst x) $startpos $endpos }

let binop ==
| PLUS;
    { (fun e1 e2 -> Abssyn.Arith {op= Abssyn.Plus; e1; e2}) }
| MINUS;
    { (fun e1 e2 -> Abssyn.Arith {op= Abssyn.Minus; e1; e2}) }
| MULT;
    { (fun e1 e2 -> Abssyn.Arith {op= Abssyn.Mult; e1; e2}) }
| DIV;
    { (fun e1 e2 -> Abssyn.Arith {op= Abssyn.Div; e1; e2}) }
| LT;
    { (fun e1 e2 -> Abssyn.Comp {comp= Abssyn.Lt; e1; e2}) }
| LE;
    { (fun e1 e2 -> Abssyn.Comp {comp= Abssyn.Le; e1; e2}) }
| EQ;
    { (fun e1 e2 -> Abssyn.Eq (e1, e2)) }

let unary ==
| NEG;
    { (fun e -> Abssyn.Neg e) }
| NOT;
    { (fun e -> Abssyn.Not e) }
| ISVOID;
    { (fun e -> Abssyn.IsVoid e) }

let binding :=
| id = OBJECTID; COLON; typ = TYPEID; ~ = init;
    { (Tables.make_id id, Tables.make_type typ, init, $startpos, $endpos) }

let branch :=
| id = OBJECTID; COLON; typ = TYPEID; DARROW; body = expr; SEMI;
    { {Abssyn.elem= (create_var_decl ~id ~typ, body); startpos= $startpos; endpos= $endpos} }

let args := LPAREN; ~ = separated_list(COMMA, expr); RPAREN;
    <>
