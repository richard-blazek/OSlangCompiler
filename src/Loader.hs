module Loader (load) where

import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified Data.Map as Map
import qualified Parser

makeImportsAbsolute :: String -> Parser.File -> IO Parser.File
makeImportsAbsolute anchor (Parser.File declarations) = fmap Parser.File replaced
  where toAbsolute = Dir.makeAbsolute . Path.combine (Path.takeDirectory anchor)
        makeAbsolute (Parser.Import path) = fmap Parser.Import (toAbsolute path)
        makeAbsolute x = return x
        replaced = sequence (map makeAbsolute declarations)

parseFile :: String -> IO Parser.File
parseFile path = fmap Parser.parse (readFile path) >>= makeImportsAbsolute path

imports :: Parser.File -> [String]
imports (Parser.File declarations) = concat (map getPaths declarations)
  where getPaths (Parser.Import path) = [path]
        getPaths _ = []

processFiles :: [String] -> Map.Map String Parser.File -> IO (Map.Map String Parser.File)
processFiles remaining done = return done

load :: String -> IO (Map.Map String Parser.File)
load path = Dir.makeAbsolute path >>= (\p -> processFiles [p] Map.empty)

