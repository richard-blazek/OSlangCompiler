module Parser (Value (..), Declaration (..), Mutability (..), Visibility (..), File, parse) where

import Lexer

type File = [Declaration]
data Visibility = Private | Public deriving (Show, Eq, Ord)
data Mutability = Mutable | Constant deriving (Show, Eq, Ord)

data Declaration
  = Global Visibility Mutability String Value
  | Record Visibility String [(String, Value)]
  | Import String deriving (Show, Eq, Ord)

data Value
  = Int Integer
  | String String
  | Bool Bool
  | Name String
  | Access Value String
  | Operation String [Value]
  | Assignment Value Value
  | Function [(String, Value)] Value deriving (Show, Eq, Ord)

parseFactor :: [Token] -> (Value, [Token])
parseFactor (Number int : tokens) = (Int int, tokens)
parseFactor (Text str : tokens) = (String str, tokens)
parseFactor (Word "true" : tokens) = (Bool True, tokens)
parseFactor (Word "false" : tokens) = (Bool False, tokens)
parseFactor (Word "fun" : tokens) = parseFunction tokens
parseFactor (Word "if" : tokens) = parseIf tokens
parseFactor (Word "while" : tokens) = parseWhile tokens
parseFactor (Word name : tokens) = (Name name, tokens)
parseFactor (Operator "(" : tokens) = let (val, Operator ")" : rest) = parseValue tokens in (val, rest)

expect :: Token -> [Token] -> [Token]
expect token (first : tokens) | first == token = tokens

parseBlock :: [Token] -> (Value, [Token])
parseBlock = parse []
  where parse statements tokens@(Word kw : _) | kw `elem` ["end", "else", "elif"] = (Operation ";" (reverse statements), tokens)
        parse statements tokens = let (st, rest) = parseValue tokens in parse (st : statements) rest

parseList :: ([Token] -> (a, [Token])) -> [Token] -> ([a], [Token])
parseList parseItem tokens = parseItems [] (Operator "," : tokens)
  where parseItems items (Operator "," : tokens) = let (item, rest) = parseItem tokens in parseItems (item : items) rest
        parseItems items (Operator "]" : tokens) = (reverse items, tokens)

parseArgList :: [Token] -> ([(String, Value)], [Token])
parseArgList (Operator "[" : tokens) = (concat argLoL, rest)
  where parseName (Word name : tokens) = (name, tokens)
        parseNames tokens = let (names, Operator ":" : rest) = parseList parseName tokens in (names, rest)
        parseArg tokens = let (names, tokens2) = parseNames tokens in let (tp, rest) = parseValue tokens in (map (\n -> (n, tp)) names, rest)
        (argLoL, rest) = parseList parseArg tokens

parseFunction :: [Token] -> (Value, [Token])
parseFunction tokens = (Function arguments block, tokens3)
  where (arguments, tokens2) = parseArgList tokens
        (block, Word "end" : tokens3) = parseBlock tokens2

parseIf :: [Token] -> (Value, [Token])
parseIf tokens
  | end == "end" = (Operation "if" [cond, block, Operation ";" []], tokens3)
  | end == "elif" = let (alt, tokens4) = parseIf tokens3 in (Operation "if" [cond, block, alt], tokens4)
  | end == "else" = let (alt, tokens4) = parseBlock tokens3 in (Operation "if" [cond, block, alt], tokens4)
  where (cond, tokens2) = parseValue tokens
        (block, Word end : tokens3) = parseBlock tokens2

parseWhile :: [Token] -> (Value, [Token])
parseWhile tokens = (Operation "while" [condition, block], tokens3)
  where (condition, tokens2) = parseValue tokens
        (block, tokens3) = parseBlock tokens2

parseAccess :: [Token] -> (Value, [Token])
parseAccess tokens = uncurry parseOp (parseFactor tokens)
  where parseArgs = parseList parseValue
        parseOp fun (Operator "[" : tokens) = let (args, rest) = parseArgs tokens in parseOp (Operation "[]" (fun : args)) rest
        parseOp obj (Operator "." : Word name : tokens) = parseOp (Access obj name) tokens
        parseOp ptr (Operator "$" : tokens) = parseOp (Operation "$" [ptr]) tokens
        parseOp value tokens = (value, tokens)

parseMinus :: [Token] -> (Value, [Token])
parseMinus (Operator "~" : tokens) = let (value, rest) = parseFactor tokens in (Operation "~" [value], rest)
parseMinus (Operator "!" : tokens) = let (value, rest) = parseFactor tokens in (Operation "!" [value], rest)
parseMinus tokens = parseFactor tokens

parseNext :: [String] -> ([Token] -> (Value, [Token])) -> (Value, [Token]) -> (Value, [Token])
parseNext ops subparse (left, Operator op : tokens) | op `elem` ops = (result, tokens3)
  where (right, tokens2) = subparse tokens
        (result, tokens3) = parseNext ops subparse (Operation op [left, right], tokens2)
parseNext ops subparse (value, tokens) = (value, tokens)

parseMul :: [Token] -> (Value, [Token])
parseMul = parseNext ["*", "/", "%", "<<", ">>"] parseMinus . parseMinus

parseAdd :: [Token] -> (Value, [Token])
parseAdd = parseNext ["*", "/", "%", "&", "|", "^"] parseMul . parseMul

parseCmp :: [Token] -> (Value, [Token])
parseCmp = parseStep Nothing . parseAdd
  where isCmp = (`elem` ["==", "!=", ">", "<", ">=", "<="])
        makeCmp op first second = Operation op [first, second]
        makeAnd op last first second = Operation "&" [first, Operation op [last, second]]
        parseOne (value, tokens) makeOp = parseStep (Just value) (makeOp value, tokens)
        parseStep Nothing (val, Operator op : tokens) | isCmp op = parseOne (parseAdd tokens) (makeCmp op val)
        parseStep (Just last) (val, Operator op : tokens) | isCmp op = parseOne (parseAdd tokens) (makeAnd op last val)
        parseStep _ (val, tokens) = (val, tokens)

parseValue :: [Token] -> (Value, [Token])
parseValue = parseAsgn . parseCmp
  where parseAsgn (left, Operator "<-" : tokens) = let (right, rest) = parseValue tokens in (Assignment left right, rest)
        parseAsgn (left, tokens) = (left, tokens)

parseNewDecl :: Visibility -> [Token] -> (Declaration, [Token])
parseNewDecl v (Word "record" : Word name : tokens) = let (field, rest) = parseArgList tokens in (Record v name field, rest)
parseNewDecl v (Word name : Operator "<-" : tokens) = let (value, rest) = parseValue tokens in (Global v Mutable name value, rest)
parseNewDecl v (Word name : Operator "==" : tokens) = let (value, rest) = parseValue tokens in (Global v Constant name value, rest)

parseDeclaration :: [Token] -> (Declaration, [Token])
parseDeclaration (Word "import" : Text path : tokens) = (Import path, tokens)
parseDeclaration (Word "private" : tokens) = parseNewDecl Private tokens
parseDeclaration tokens = parseNewDecl Public tokens

parseFile :: [Declaration] -> [Token] -> File
parseFile declarations [] = declarations
parseFile declarations tokens = let (decl, rest) = parseDeclaration tokens in parseFile (decl : declarations) rest

parse :: String -> File
parse = parseFile [] . tokenise
