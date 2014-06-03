module Main where

import System.IO
import System.Cmd (system)
import GHC.IO.Exception (ExitCode(..) )

import Lexer (alexScanTokens, Token (..))
import Parser (parse)
import Syntax
import Semantics (eval_exp)

import qualified Data.Map as M

grabLines' tokens' 0 = return tokens'
grabLines' tokens' n =
    do
    str <- getLine
    tokens <- return $ alexScanTokens str
    grabLines'
        (tokens' ++ tokens)
        (foldl
            (\x y ->
             case y of
                TokenLBracket -> x + 1
                TokenRBracket -> x - 1
                _ -> x
            )
            n
            tokens
        )

grabLines =
    do
    str <- getLine
    tokens <- return $ alexScanTokens str
    grabLines'
        tokens
        (foldl
            (\x y ->
             case y of
                TokenLBracket -> x + 1
                TokenRBracket -> x - 1
                _ -> x
            )
            0
            tokens
        )

main :: IO ()
main =
    do
    putStr "> "
    hFlush stdout
    tok <- grabLines
    e <- return $ parse tok
    putStr $ show e++"\n" -- Prints AST

    --case typecheck_exp e [] Map.empty of
    --   (None, _)    -> putStr "Type Error\n"
    --   _            -> putStr $ show (eval_exp e [] Map.empty) ++"\n" -- Prints the return

    putStr $ show (eval_exp e [] M.empty) ++ "\n"
    main