module Main where

import System.Environment
import System.Exit
import Text.Read

type Size = Int

main :: IO ()
main = do
    size <- validateInput
    putStr $ unlines $ diamond size

diamond :: Size -> [String]
diamond size =
    let top    = diamondLine '/' '\\' size <$> [1..size]
        bottom = diamondLine '\\' '/' size <$> [1..size]
    in reverse top ++ bottom

diamondLine :: Char -> Char -> Size -> Int -> String
diamondLine lchar rchar size line =
        let lspaces = line - 1
            rspaces = line - 1
            midspaces = size * 2 - lspaces - rspaces - 2
        in
               replicate lspaces ' '
            ++ [lchar]
            ++ replicate midspaces ' '
            ++ [rchar]
            ++ replicate rspaces ' '

validateInput :: IO Size
validateInput = do
    args <- getArgs
    case args of
        [size] -> case readMaybe size of
            Just s
                | s < 1     -> fail "Invalid size"
                | otherwise -> pure s
            Nothing -> fail "Could not parse size"
        _ -> do
            pn <- getProgName
            putStrLn $ "Usage: " ++ pn ++ " <size>"
            exitFailure
