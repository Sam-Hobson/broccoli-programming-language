module Main where
import System.Environment (getArgs)
import Parser
import ModuleParser (codeModule)

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
        Right a -> putStr $ show a

    putStr $ sep ++ "REMAINING CONTENT" ++ sep

    case getRemainder parsedContent of
        Left a -> putStr $ "Error: " ++ show a
        Right a -> putStr $ show a
