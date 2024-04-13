module Typing (Type (..), isInstantiable, isBuiltin, access, value, function, operator) where

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

isInstantiable :: Type -> Bool
isInstantiable (Type t) = False
isInstantiable t = t /= Void

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

isInteger :: Type -> Bool
isInteger = (`elem` [Int8, Int16, Int32, Int64])

isLogical :: Type -> Bool
isLogical = (`elem` [Int8, Int16, Int32, Int64, Bool])

isIntegral :: Type -> Bool
isIntegral (Pointer _) = True
isIntegral t = isLogical t

function :: String -> [Type] -> Type
function fn args = case (fn, args) of
  ("Fun", args) -> let types = map (\(Type t) -> t) args in Type (Function (init args) (last args))
  ("Ptr", [Type t]) -> Type (Pointer t)
  ("cast", [Type t, u]) | isIntegral t && isIntegral u -> t

operator :: String -> [Type] -> Type
operator op args = case (op, args) of
  ("if", [Bool, t, u]) -> if t == u then t else Void
  ("while", [Bool, t]) -> Void
  ("[]", (Function args result : rest)) | args == rest -> result
  ("$", [Pointer t]) -> t
  ("~", [x]) | isInteger x -> x
  ("!", [x]) | isLogical x -> x
  ("+", [Pointer t, u]) | isInteger u -> Pointer t
  ("+", [t, u]) | t == u && isInteger t -> t
  ("-", [Pointer t, Pointer u]) | t == u -> Pointer t
  ("-", [Pointer t, u]) | isInteger u -> Pointer t
  ("-", [t, u]) | t == u && isInteger t -> t
  ("*", [t, u]) | t == u && isInteger t -> t
  ("/", [t, u]) | t == u && isInteger t -> t
  ("%", [t, u]) | t == u && isInteger t -> t
  ("<<", [t, u]) | t == u && isInteger t -> t
  (">>", [t, u]) | t == u && isInteger t -> t
  ("&", [t, u]) | t == u && isLogical t -> t
  ("|", [t, u]) | t == u && isLogical t -> t
  ("^", [t, u]) | t == u && isLogical t -> t
  ("==", [t, u]) | t == u -> Bool
  ("!=", [t, u]) | t == u -> Bool
  ("<", [t, u]) | t == u && isIntegral t -> Bool
  (">", [t, u]) | t == u && isIntegral t -> Bool
  ("<=", [t, u]) | t == u && isIntegral t -> Bool
  (">=", [t, u]) | t == u && isIntegral t -> Bool
  ("<-", [Pointer t, u]) | t == u -> Void
