module Evaluator (eval) where

import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Debug.Trace (traceShowId)
import Tree (Expr (..), Param (..))
import qualified Tree

data HExpr
  = HVar Text
  | HLam (HExpr -> IO HExpr)
  | HApp HExpr HExpr
  | HBool Bool
  | HInt Int
  | HUnit

instance Show HExpr where
  show (HVar n) = unpack n
  show (HBool b) = show b
  show (HInt i) = show i
  show (HApp _ _) = "<app>"
  show (HLam _) = "<function>"
  show HUnit = "()"

type Context = [(Text, HExpr)]

call :: HExpr -> [HExpr] -> IO HExpr
call (HLam f) [] = error "Arity incorrect"
call (HLam f) (x : xs) = do
  e <- f x
  call e xs
call e [] = pure e

eval :: Context -> Expr -> IO HExpr
eval ctx = \case
  ELet {name, value, next} -> do
    e <- eval ctx next
    eval ((Tree.paramText name, e) : ctx) next
  EFunction {params, value, location} ->
    case params of
      [y] ->
        pure $
          HLam
            ( \u -> do
                eval ((Tree.paramText y, u) : ctx) value
            )
      (x : xs) ->
        pure $
          HLam
            ( \u ->
                eval
                  ((Tree.paramText x, u) : ctx)
                  (EFunction {params = xs, value = value, location = location})
            )
      [] -> pure $ HLam (const $ eval ctx value)
  EIf {condition, thenBranch, elseBranch} -> do
    e <- eval ctx condition
    case e of
      HBool cond ->
        if cond
          then eval ctx thenBranch
          else eval ctx elseBranch
      _ -> error "Condition is not a boolean"
  EBinary {lhs, op, rhs} -> do
    (HInt l) <- eval ctx lhs
    (HInt r) <- eval ctx rhs
    case op of
      "Add" -> pure $ HInt (l + r)
      "Sub" -> pure $ HInt (l - r)
      "Lt" -> pure $ HBool (l < r)
      _ -> error "Operation not recognized"
  EVar {text} -> do
    case lookup text ctx of
      Nothing -> error $ "Variable '" ++ unpack text ++ "' not found!"
      Just ex -> pure ex
  ECall {callee, arguments} -> do
    e <- eval ctx callee
    es <- traverse (eval ctx) arguments
    call e es
  EInt {intValue} -> pure $ HInt intValue
  EPrint {value} -> do
    print =<< eval ctx value
    pure HUnit
