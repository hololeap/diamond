module Main where

import System.Environment
import System.Exit
import Text.Read

type Size = Int

data Line = Line
    { maxWidth :: Int
    , lineNumber :: Int
    }

main :: IO ()
main = do
    size <- validateInput
    putStr $ unlines $ diamond size

diamond :: Size -> [String]
diamond size =
    let maxWidth = if odd size then size else size - 1
    in  diamondLine <$> diamondTop maxWidth size ++ diamondBottom maxWidth size

diamondTop :: Int -> Size -> [Line]
diamondTop maxWidth size
    | odd size  = mkLines $ size `quot` 2 + 1
    | otherwise = mkLines $ size `quot` 2
  where
    mkLines :: Int -> [Line]
    mkLines topHeight = Line maxWidth <$> [1..topHeight]

diamondBottom :: Int -> Size -> [Line]
diamondBottom maxWidth size =
    let bottomHeight = size `quot` 2
    in Line maxWidth <$> [bottomHeight, bottomHeight - 1 .. 1]

diamondLine :: Line -> String
diamondLine (Line maxWidth lineNumber) =
    let stars = lineNumber * 2 - 1
        spaces = (maxWidth - stars) `quot` 2
    in
           replicate spaces ' '
        ++ replicate stars  '*'
        ++ replicate spaces ' '

validateInput :: IO Size
validateInput = do
    args <- getArgs
    case args of
        [size] -> case readMaybe size of
            Just s
                | s < 0     -> fail "Invalid size"
                | otherwise -> pure s
            Nothing -> fail "Could not parse size"
        _ -> do
            pn <- getProgName
            putStrLn $ "Usage: " ++ pn ++ " <size>"
            exitFailure


-- # for i in {0..10}; do echo $i; ./diamond-v2 $i; echo;  done
-- 0
-- 
-- 1
-- *
-- 
-- 2
-- *
-- *
-- 
-- 3
--  * 
-- ***
--  * 
-- 
-- 4
--  * 
-- ***
-- ***
--  * 
-- 
-- 5
--   *  
--  *** 
-- *****
--  *** 
--   *  
-- 
-- 6
--   *  
--  *** 
-- *****
-- *****
--  *** 
--   *  
-- 
-- 7
--    *   
--   ***  
--  ***** 
-- *******
--  ***** 
--   ***  
--    *   
-- 
-- 8
--    *   
--   ***  
--  ***** 
-- *******
-- *******
--  ***** 
--   ***  
--    *   
-- 
-- 9
--     *    
--    ***   
--   *****  
--  ******* 
-- *********
--  ******* 
--   *****  
--    ***   
--     *    
-- 
-- 10
--     *    
--    ***   
--   *****  
--  ******* 
-- *********
-- *********
--  ******* 
--   *****  
--    ***   
--     *    
-- 

