module Loader (load) where

import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Parser

absolutisePath :: String -> String -> IO String
absolutisePath dir relpath = Dir.makeAbsolute (Path.combine dir relpath)

absolutiseDecl :: String -> Parser.Declaration -> IO Parser.Declaration
absolutiseDecl dir (Parser.Import path) = fmap Parser.Import (absolutisePath dir path)
absolutiseDecl dir x = return x

absolutiseFile :: String -> Parser.File -> IO Parser.File
absolutiseFile dir file = sequence (map (absolutiseDecl dir) file)

parseFile :: String -> IO Parser.File
parseFile path = fmap Parser.parse (readFile path) >>= absolutiseFile (Path.takeDirectory path)

imports :: Parser.File -> Set.Set String
imports file = Set.unions (map pathsOf file)
  where pathsOf (Parser.Import path) = Set.singleton path
        pathsOf _ = Set.empty

processFiles :: Map.Map String Parser.File -> Set.Set String -> IO (Map.Map String Parser.File)
processFiles done paths | Set.null paths = return done
processFiles done paths = do
  let (path, otherPaths) = Set.deleteFindMin paths
  file <- parseFile path
  let newPaths = Set.union otherPaths (Set.filter (`Map.notMember` done) (imports file))
  processFiles (Map.insert path file done) newPaths

load :: String -> IO (Map.Map String Parser.File)
load path = Dir.makeAbsolute path >>= processFiles Map.empty . Set.singleton
