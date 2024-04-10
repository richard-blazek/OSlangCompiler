module Language (Type (..), isType, isInstantiable, isBuiltin, access, value, function, operator) where

import Data.Map.Strict (Map, (!))

data Type
  = Void
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Pointer Type
  | Function [Type] Type
  | Record String (Map String Type)
  | Type Type deriving (Show, Eq, Ord)

isType :: Type -> Bool
isType (Type t) = True
isType _ = False

isInstantiable :: Type -> Bool
isInstantiable t = not (isType t) && t /= Void

isBuiltin :: String -> Bool
isBuiltin = (`elem` ["Void", "Bool", "I8", "I16", "I32", "I64", "Ptr", "Fun"])

access :: String -> Type -> Type
access name (Record _ fields) = fields ! name
access name (Pointer (Record _ fields)) = Pointer (fields ! name)

value :: String -> Type
value "Void" = Type Void
value "Bool" = Type Bool
value "I8" = Type Int8
value "I16" = Type Int16
value "I32" = Type Int32
value "I64" = Type Int64

function :: String -> [Type] -> Type
function fn args = case (fn, args) of
  ("Fun", args) -> let types = map (\(Type t) -> t) args in Type (Function (init args) (last args))
  ("Ptr", [Type t]) -> Type (Pointer t)

operator :: String -> [Type] -> Type
operator op args = case (op, args) of
  ("if", [Bool, t, u]) -> if t == u then t else Void
  ("while", [Bool, t]) -> Void
  ("[]", (Function args result : rest)) | args == rest -> result
  ("$", [Pointer t]) -> t
  ("~", [x]) | x `elem` integer -> x
  ("!", [x]) | x `elem` logical -> x
  ("+", [Pointer t, u]) | u `elem` integer -> Pointer t
  ("+", [t, u]) | t == u && t `elem` integer -> t
  ("-", [Pointer t, Pointer u]) | t == u -> Pointer t
  ("-", [Pointer t, u]) | u `elem` integer -> Pointer t
  ("-", [t, u]) | t == u && t `elem` integer -> t
  ("*", [t, u]) | t == u && t `elem` integer -> t
  ("/", [t, u]) | t == u && t `elem` integer -> t
  ("%", [t, u]) | t == u && t `elem` integer -> t
  ("&", [Pointer t, u]) | t == u && t `elem` logical -> t
  ("&", [t, u]) | t == u && t `elem` logical -> t
  ("|", [t, u]) | t == u && t `elem` logical -> t
  ("^", [t, u]) | t == u && t `elem` logical -> t
  ("==", [t, u]) | t == u -> Bool
  ("!=", [t, u]) | t == u -> Bool
  ("<", [t, u]) | t == u && t `elem` integer -> Bool
  ("<", [Pointer t, Pointer u]) | t == u -> Bool
  (">", [t, u]) -> operator "<" [t, u]
  ("<=", [t, u]) -> operator "<" [t, u]
  (">=", [t, u]) -> operator "<" [t, u]
  where integer = [Int8, Int16, Int32, Int64]
        logical = [Int8, Int16, Int32, Int64, Bool]
