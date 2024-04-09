module Main where

import qualified System.IO as SysIO
import qualified System.Environment as Env

import Parser (parse)

compile :: [String] -> IO ()
compile (input : output : args) = readFile input >>= SysIO.writeFile output . show . parse

main :: IO ()
main = Env.getArgs >>= compile
