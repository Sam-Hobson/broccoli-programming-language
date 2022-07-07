module Main where

import Interpreter (interpret)
import ModuleParser (codeModule)
import ParseTypes (Statement (..))
import Parser
import System.Environment (getArgs)
import GlobalScope

formatParse1 :: Int -> [Statement] -> String
formatParse1 n =
  mconcat
    . map
      ( \x -> case x of
          FD (a, b, c, d) ->
            replicate n '\t' ++ show (a, b, c)
              ++ " {\n"
              ++ formatParse1 (n + 1) d
              ++ replicate n '\t'
              ++ "}\n"
          _ -> replicate n '\t' ++ show x ++ "\n"
      )

formatParse :: [Statement] -> String
formatParse = formatParse1 0

main :: IO ()
main = do
  let sep = "\n-----------------------------------------------------------\n"

  filepath <- head <$> getArgs
  content <- readFile filepath

  putStr $ sep ++ "FILE CONTENT" ++ sep ++ content

  putStr $ sep ++ "PARSED CONTENT" ++ sep

  let parsedContent = parse codeModule content

  case getParsed parsedContent of
    Left a -> putStr $ "Error: " ++ show a
    Right a -> do
        putStr $ concatMap ((++"\n") . show) a
        putStr $ sep ++ "INTERPRETED" ++ sep
        let (io, sd, _) = interpret global a
        -- print sd
        io

  putStr $ sep ++ "REMAINING CONTENT" ++ sep

  case getRemainder parsedContent of
    Left a -> putStr $ "Error: " ++ show a
    Right a -> putStr $ show a

