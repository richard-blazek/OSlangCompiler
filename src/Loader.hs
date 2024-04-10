module Loader (load) where

import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Parser

absolutiseImports :: String -> Parser.File -> IO Parser.File
absolutiseImports anchor (Parser.File declarations) = fmap Parser.File replaced
  where absPath = Dir.makeAbsolute . Path.combine (Path.takeDirectory anchor)
        absolutise (Parser.Import path) = fmap Parser.Import (absPath path)
        absolutise x = return x
        replaced = sequence (map absolutise declarations)

parseFile :: String -> IO Parser.File
parseFile path = fmap Parser.parse (readFile path) >>= absolutiseImports path

imports :: Parser.File -> Set.Set String
imports (Parser.File declarations) = Set.unions (map getPaths declarations)
  where getPaths (Parser.Import path) = Set.singleton path
        getPaths _ = Set.empty

processFiles :: Map.Map String Parser.File -> Set.Set String -> IO (Map.Map String Parser.File)
processFiles done paths
  | Set.null paths = return done
  | otherwise = do
    let (path, otherPaths) = Set.deleteFindMin paths
    parsedFile <- parseFile path
    let newDone = Map.insert path parsedFile done
    let newPaths = Set.union otherPaths (Set.filter (`Map.notMember` done) (imports parsedFile))
    processFiles newDone newPaths

load :: String -> IO (Map.Map String Parser.File)
load path = Dir.makeAbsolute path >>= processFiles Map.empty . Set.singleton
