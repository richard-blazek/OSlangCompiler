module Typing (Type (..), isInstantiable, access, globals, operator) where

import Data.Map.Strict (Map, (!), fromList)

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
  | Type Type
  | Builtin String deriving (Show, Eq, Ord)

isInstantiable :: Type -> Bool
isInstantiable (Type t) = False
isInstantiable t = t /= Void

access :: String -> Type -> Type
access name (Record _ fields) = fields ! name
access name (Pointer (Record _ fields)) = Pointer (fields ! name)

globals :: Map String Type
globals = fromList [
  ("Void", Type Void), ("Bool", Type Bool),
  ("I8", Type Int8), ("I16", Type Int16), ("I32", Type Int32), ("I64", Type Int64),
  ("Fun", Builtin "Fun"), ("Ptr", Builtin "Ptr"), ("cast", Builtin "cast")]

operator :: String -> [Type] -> Type
operator op args = case (op, args) of
  ("if", [Bool, t, u]) -> if t == u then t else Void
  ("while", [Bool, t]) -> Void
  ("[]", (Function args result : rest)) | args == rest -> result
  ("[]", (Builtin "Fun" : args)) -> let types = map (\(Type t) -> t) args in Type (Function (init types) (last types))
  ("[]", [Builtin "Ptr", Type t]) -> Type (Pointer t)
  ("[]", [Builtin "cast", Type t, u]) | isIntegral t && isIntegral u -> t
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

  where isInteger = (`elem` [Int8, Int16, Int32, Int64])
        isLogical = (`elem` [Int8, Int16, Int32, Int64, Bool])
        isIntegral (Pointer _) = True
        isIntegral t = isLogical t
