import Cookbook.Essential.IO
import Cookbook.Essential.Common
import Cookbook.Project.Quill.Quill

import qualified System.IO        as LIO
import qualified System.IO.Strict as SIO

import System.Environment

gfilename = ".schedule"

main = do  
  file   <- inhome gfilename LIO.ReadMode
  flines <- fmap lines $ SIO.hGetContents file
  args   <- getArgs

  case args of
    (command:name:diff:desc:[]) -> writeNew name diff desc
    (command:arg:[])            -> if (command == "read") then readSch arg flines else writeSch arg flines
    (list:_)                    -> listSch flines

readSch :: String -> [String] -> IO ()
readSch arg schLines =
  do
    let allTables   = tables schLines
    let appropTable = getTable allTables arg
    mapM_ prettyPrint appropTable

writeSch :: String -> [String] -> IO ()
writeSch arg schLines =
  do
    name        <- prompt "Name: "
    difficulty  <- prompt "Difficulty: "
    description <- prompt "Description: "

    writeNew name difficulty description

listSch :: [String] -> IO ()
listSch files =
  do
    let allTables = tables files
    mapM_ putStrLn $ map fst allTables

writeNew :: String -> String -> String -> IO ()
writeNew name difficulty description =
  do
    flines <- (inhome gfilename) LIO.ReadMode >>= SIO.hGetContents
    
    let tblAddedNm = createTable (tables (lines flines)) name
    let tblAddedDs = addItem tblAddedNm name ("difficulty" ,difficulty)
    let tblAddedDc = addItem tblAddedDs name ("description",description)
--Could be done in a map, but why? It's only 3 lets.        
    writeFile gfilename $ flt $ map tableToString tblAddedDc

prettyPrint :: (String,String) -> IO ()
prettyPrint (a,b) = putStrLn (a ++ " : " ++ b)
