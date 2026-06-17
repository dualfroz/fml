module Fml.Core.Ast

// Abstract syntax for the fml language. Multi-argument functions are
// desugared into nested single-argument lambdas by the parser, so the
// evaluator and type checker only ever see one parameter per lambda.

type Expr =
    | IntLit of int
    | BoolLit of bool
    | StringLit of string
    | UnitLit
    | Var of string
    | Lambda of string * Expr
    | App of Expr * Expr
    | Let of bool * string * Expr * Expr
    | LetGroup of bool * (string * Expr) list * Expr
    | If of Expr * Expr * Expr
    | BinOp of string * Expr * Expr
    | Neg of Expr
    | Tuple of Expr list
    | Nil
    | Cons of Expr * Expr
    | Match of Expr * (Pattern * Expr) list
    | Construct of string * Expr list
    | Record of (string * Expr) list
    | Field of Expr * string

and Pattern =
    | PVar of string
    | PWildcard
    | PInt of int
    | PBool of bool
    | PString of string
    | PUnit
    | PTuple of Pattern list
    | PNil
    | PCons of Pattern * Pattern
    | PConstruct of string * Pattern list
    | PRecord of (string * Pattern) list

// Surface syntax for type expressions inside declarations and annotations.
type TyExpr =
    | TEVar of string
    | TECon of string * TyExpr list
    | TETuple of TyExpr list
    | TEArrow of TyExpr * TyExpr
    | TEList of TyExpr

type TypeBody =
    | Variants of (string * TyExpr list) list
    | RecordDef of (string * TyExpr) list

type TypeDecl =
    { Name: string
      Params: string list
      Body: TypeBody }

// A top-level phrase is either a bare expression, a definition group, a type
// declaration group, a module, or an open directive. Definitions extend the
// persistent session environment.
type Phrase =
    | PhraseExpr of Expr
    | PhraseLet of bool * (string * Expr) list
    | PhraseType of TypeDecl list
    | PhraseModule of string * Phrase list
    | PhraseOpen of string
