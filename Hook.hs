import Cookbook.Ingredients.Lists.Modify
import Cookbook.Ingredients.Lists.Access
import Cookbook.Ingredients.Tupples.Look
import Cookbook.Recipes.Sanitize
import Cookbook.Continuous
import Cookbook.IO

import System.IO
import System.Environment
import System.Process

import Data.Maybe
--General form of an entry:
-- ***level:job

isMission :: String -> Bool
isMission x = and [(':' `elem` x),(x `contains` "***")]

getDifficulty :: String -> String
getDifficulty x = before (after (rm (before x ':') ' ') "***") ':'

getJob :: String -> String
getJob = (flip after) ':'

main = do
  (file:command:argument:_) <- getArgs
  allLines <- filelines file

  let parsed = catMaybes $ map (\c -> if isMission c then (Just ((getDifficulty c),(getJob c))) else Nothing) allLines
               
  case command of "add" -> appendFile file argument;"strike" -> strike file argument;"list" -> list parsed argument

-- | Strike the line containing the argument from the file.
strike :: String -> String -> IO ()
strike file arg = do
  a <- filelines file
  system $  "put " ++ file ++ " " ++ (flt [surround d ('"','"')++" " | d <- a, (not (d `contains` arg))])
  return ()

list :: [(String,String)] -> String -> IO ()
list [] _ = return ()
list ((a,b):c) "all" = do putStrLn $ "Job: " ++ b ++ ", Difficulty:: " ++ a;
                                 list c "all"
list parsed arg = mapM_ diffSpeak $ (filter (\(diff,jb) -> or [diff `contains` arg, jb `contains` arg]) parsed)

surround :: [a] -> (a,a) -> [a]
surround a (b,c) = b:a ++ [c]

flt :: [[a]] -> [a]
flt [] = []
flt (x:xs) = x ++ flt xs

diffSpeak (a,b) = putStrLn ("Job: " ++ b ++ ", Difficulty: " ++ a)
