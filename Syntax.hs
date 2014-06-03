module Syntax where

import Memory
import Env

data ASTExpression =
    Num Int
  | Str String
  | Id String
  | Lambda [(String, Type)] ASTExpression
  | Apply ASTExpression [ASTExpression]
  | Add ASTExpression ASTExpression
  | Subtract ASTExpression ASTExpression
  | Multiply ASTExpression ASTExpression
  | Divide ASTExpression ASTExpression
  | Deref ASTExpression
  | Let ASTExpression ASTExpression
  | Lt ASTExpression ASTExpression
  | Gt ASTExpression ASTExpression
  | Get ASTExpression ASTExpression
  | Eq ASTExpression ASTExpression
  | VTrue
  | VFalse
  | And ASTExpression ASTExpression
  | Or ASTExpression ASTExpression
  | Not ASTExpression
  | Ternary ASTExpression ASTExpression ASTExpression
  | Alloc ASTExpression
  | LDecl [(String, ASTExpression)] ASTStatement ASTExpression
  deriving(Eq,Show)

data ASTStatement =
    While ASTExpression ASTStatement
  | Seq ASTStatement ASTStatement
  | Call ASTExpression [ASTExpression]
  | Free ASTExpression
  | If ASTExpression ASTStatement
  | IfElse ASTExpression ASTStatement ASTStatement
  | Assign ASTExpression ASTExpression
  | Print ASTExpression
  | Println
  | SDecl [(String, ASTExpression)] ASTStatement
  deriving (Eq,Show)

data Result =
    Integer Int
  | Boolean Bool
  | String String
  | Reference Ref
  | Closure [String] ASTExpression (Env Result)
  | Delay ASTExpression (Env Result)
  | DelayState ASTStatement (Env Result)
  | Undefined
  deriving(Eq,Show)

data Type =
    IntType
  | BoolType
  | StringType
  | RefType Type
  | FunType [Type]
  | None
  deriving(Eq,Show)
