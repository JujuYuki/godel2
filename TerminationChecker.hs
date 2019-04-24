
{-# LANGUAGE DeriveDataTypeable,  BangPatterns #-}


import GoParser
import TermConverter
--import Printer

import Data.List as L



import System.Environment
import System.FilePath.Posix
import System.Process
import System.Console.CmdArgs
import Control.Monad
import Data.Text (strip, pack, unpack,lines)
import System.Console.ANSI
import System.FilePath

data GenMode = Termination
             | Deadlock
             | Terminal
             | Liveness
             | Safety
             | All
                 deriving (Data,Typeable,Show,Eq)

data Checker = Checker
               { check :: GenMode
               , gofile :: String
               , strategy :: String
               , debug :: Bool
               }
             deriving (Data,Typeable,Show,Eq)

submodes =  enum
            [ All
              &= help "Check all properties"
              &= name "A"
            , Deadlock
              &= help "Checks whether the model can reach a deadlock state\n(True = no deadlock)"
              &= name "D"
            , Terminal
              &= help "Checks whether the model can reach a terminal state\n(True = no terminal state)"
              &= name "G"
            , Liveness
              &= help "Checks whether the model is live\n(True = live)"
              &= name "L"
            , Safety
              &= help "Checks whether the model is safe\n(True = live)"
              &= name "S"
            , Termination
              &= help "Check termination"
              &= name "T"
            ]

subargs = Checker { check = submodes
                  , gofile = def &= argPos 0 &= typFile
                  , strategy = def 
                               &= typ "regular | regular2 | stack"
                               &= explicit &= name "strat"
                               &= opt "stack"
                  , debug = def 
                            &= explicit &= name "debug"
                  }
          &= help "Go-Types model checking (via mCRL2)"
  


writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content


main :: IO ()
main = do
     pargs <- cmdArgs (modes [subargs])
     tyfile <- (readFile $ gofile pargs)
     case parseprog tyfile of
          Left err -> print err
          Right ty -> do let termStruct = processProg ty
                             prettyIf = show termStruct
                             fors = filterFors termStruct
                             expanded = genExpansion fors
                             cCode = genCCode expanded
                             (m,l) = genMain expanded
                             fname = gofile pargs
                             deb = debug pargs
                             mainCalls = getMainCalls expanded

                         when ((check pargs) ==  Termination) $ do
                           putStrLn $ "Termination Analysis:" ;
                           if (null fors) then
                             (printResult "\t No For loops, trivially " True)
                           else
                            do
                            when (deb) $ do
                                         putStrLn $ "Tree Structure: \n" ++ prettyIf ;
                                         putStrLn $ "Fors: \n" ++ (show fors) ;
                                         putStrLn $ "C-Code: \n" ++ cCode ;
                                         putStrLn $ "Main: \n" ++ m ;
                                         putStrLn $ "Calls with Parameters: "++ (show l);
                                         putStrLn $ "Calls in Main: "++(show mainCalls)
                            writeCCode fname (cCode ++ m ++ "\n") ;
                            runClang fname ;
                            res <- runTermCheck fname l deb
                            when (not res) (runStepWiseCheck fname
                                           mainCalls deb)
                           

                             


printResult :: String -> Bool -> IO()
printResult s t = do putStr $ s
                     selectColor t
                     putStrLn $ show t
                     setSGR [Reset]

-- pre: fname is path/file.cgo
writeCCode fname cCode =
  let name = dropExtension fname -- name = path/file
      target = name <.> ".c" in -- target = path/file.c
      writeToFile target cCode 
   
-- pre: fname is path/file.cgo
runClang fname =
  let cfile = (dropExtension fname) <.> ".c"
      target = (dropExtension fname) <.> ".bc"
      cmd = ("clang-4.0 -Wall -Wextra -c -emit-llvm -O0 "++ cfile  ++" -o "++ target) in
      do
       out <- readProcess "bash" ["-c",cmd] []
       if (not . null $ out) then
         error ("Error generating LLVM data: "++out)
       else
         return ()

-- pre: fname is path/file.cgo
-- f : name of function in generated c/bc
runLLVM2K fname f =
 let bcfile = (dropExtension fname) <.> ".bc"
     kfile =  (dropExtension fname) <.> ".kittel"
     fun = if (null f) then f else ("-function=" ++ f ++ " ") 
     cmd = "./llvm2kittel " ++ fun ++ bcfile ++ "> " ++ kfile in
     do
      out <- readProcess "bash" ["-c",cmd] []
      if (not . null $ out) then
        error ("Error Converting to Termination Analysis: "++out)
      else
        return ()

-- pre: fname is path/file.cgo
runKittel fname =
  let kfile = (dropExtension fname) <.> ".kittel"
      cmd = "./kittel.native " ++ kfile 
  in
    do
      out <- readProcess "bash" ["-c",cmd] []
      return $ (((elem $ "Termination successfully shown!") . L.lines $ out),out)

-- pre: fname is path/file.cgo
-- l: list of functions to test explicitly
runTermCheck fname [] debug = do
    runLLVM2K fname "" ;
    (res,out) <- runKittel fname
    printResult "\t Main: " res ;
    when (debug) (printResult ("\t"++out) res);
    return res
      
runTermCheck fname (x:xs) debug = do
    runLLVM2K fname x ;
    (res,out) <- runKittel fname
    printResult ("\t Function " ++ x ++ ": ") res ;
    when (debug) (printResult ("\t"++out) res) ;
    runTermCheck fname xs debug

runStepWiseCheck fname [] debug = return ()
runStepWiseCheck fname (x:xs) debug = do
    runLLVM2K fname x ;
    (res,out) <- runKittel fname
    printResult ("\t Function " ++ x ++ ": ") res ;
    when (debug) (printResult ("\t"++out) res) ;
    runStepWiseCheck fname xs debug  


  


selectColor :: Bool -> IO()
selectColor True =  setSGR [SetColor Foreground Vivid Green]
selectColor False =  setSGR [SetColor Foreground Vivid Red]
