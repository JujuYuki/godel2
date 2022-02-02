{-# LANGUAGE DeriveDataTypeable,  BangPatterns #-}

import GoParser
import GenModel
import GenFormula
import FlatGo
import TermConverter
import FiniteControl
import DeadcodeAnalysis (removeDeadcode)

import Data.List as L


import System.Environment
import System.FilePath.Posix
import System.Exit
import System.Process
import System.Console.CmdArgs
import Control.Monad
import Data.Text (strip, pack, unpack)
import System.Console.ANSI
import System.IO (stderr)
import System.Exit (exitSuccess)

data GenMode = Liveness
             | Deadlock
             | Terminal
             | Safety
             | DataRaceFree
             | Termination
             | All
             | Benchmarks
             | FiniteControl
             | Eventual
             deriving (Data,Typeable,Show,Eq)

data Checker = Checker
               { check :: GenMode
               , gofile :: [FilePath]
               , strategy :: String
               , chanlive :: Bool
               , debug :: Bool
               , opton :: Bool
               , ltsinfo :: Bool
               , force :: Bool
               }
             deriving (Data,Typeable,Show,Eq)
                      
submodes =  enum
            [ All
              &= help "Check all properties"
              &= name "A"
            , Benchmarks
              &= help "Check liveness and channel safety properties"
              &= name "B"
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
              &= help "Checks whether the model is safe\n(True = safe)"
              &= name "S"
            , DataRaceFree
              &= help "Checks wether the model is data race free\n(True = data race free)"
              &= name "R"
            , Termination
              &= help "Check termination"
              &= name "T"
            , Eventual
              &= help "Check eventual reception"
              &= name "E"
            , FiniteControl
              &= help "Check whether the model is finite control only"
              &= name "F"
            ]

subargs = Checker { check = submodes
                  , gofile = def &= args &= typ "FILES"
                  , strategy = def 
                               &= typ "regular | regular2 | stack"
                               &= explicit &= name "strat"
                               &= opt "stack"
                               &= help "Specify different strategies for mcrl22lps"
                  , debug = def 
                            &= explicit &= name "debug"
                            &=help "Print debug information"
                  , opton = def 
                            &= explicit &= name "opton"
                            &=help "Reduces LTS -- /!\\ May not preserve global deadlock/final state properties"
                  , chanlive = def 
                               &= explicit &= name "chanlive"
                               &= help "Prints liveness information channel by channel"
                  , ltsinfo = def
                              &= explicit &= name "ltsinfo"
                              &= help "Print LTS information (number of states, transitions, etc.)"
                  , force = def
                            &= explicit &= name "force"
                            &= help "Ignores finite control check"
                  }
          &= help "Go-Types model checking (via mCRL2)"
  
main :: IO ()
main = do pargs <- cmdArgs (modes [subargs])
          mapM_ (execute pargs) (gofile pargs)

execute :: Checker -> FilePath -> IO ()
execute pargs fname = do
  printHeader $ "File: "++fname
  tyfile <- (readFile fname)
  case parseprog tyfile of
    Left err -> print err
    Right ty -> do
      let nprog = (removeDeadcode (opton pargs) ty)
          prog = (cleanUpProg . flattenProgGo . renameProg . fixNames . sequentialiseProg) nprog
          (model, chans, taus, barbs) = genProgram prog
          termStruct = processProg prog
          prettyIf = show termStruct
          fors = filterFors termStruct
          expanded = genExpansion fors
          cCode = genCCode expanded
          (m,l) = genMain expanded
          deb = debug pargs
          mainCalls = getMainCalls expanded
          ntaus = nub taus
          chanLiveness s = do
            when (debug pargs) $
              putStrLn (liveness (Just s) ntaus barbs)
            parseModelChecker (debug pargs) False "go-model.lps" (liveness (Just s) ntaus barbs)
              >>= (printResult $ "\tLiveness for "++s++": ")
          mystrat = if null (strategy pargs)
                    then "stack"
                    else strategy pargs
          fc = checkFiniteControl nprog
          -- putStrLn $ show ty
          --    
      when (opton pargs) $ printmyprog ty nprog
        
      when ((not $ force pargs) && (check pargs) /= Termination ) $
        printResult "Finite Control:\t\t" fc
      if (not $ force pargs) && ((not fc) || (check pargs) == FiniteControl)
        then return ()
        else
               if ((check pargs) == Termination) then
                  do
                           putStrLn $ "Termination Analysis:" ;
                           if (null fors) then
                             (printResult "\t No For loops, trivially " True)
                           else
                            do
                            when (deb) $ do
                                         putStrLn $ "C-Code: \n" ++ cCode ;
                                         putStrLn $ "Main: \n" ++ m ;
                                         putStrLn $ "Calls with Parameters: "++ (show l);
                                         putStrLn $ "Calls in Main: "++(show mainCalls)
                            writeCCode fname (cCode ++ m ++ "\n") ;
                            runClang fname ;
                            res <- runTermCheck fname l deb ;
                            when (not res) (runStepWiseCheck fname
                                           mainCalls deb)  
               else
               do
                   writeToFile "model.crl2" model
                   generateLPSFile (debug pargs) "model.crl2" "go-model.lps" mystrat
                   when (ltsinfo pargs) $ do
                     generateLTSFile "go-model.lps" "go-model.lts"
                     -- callProcess "bash" ["-c","lpsinfo go-model.lps"] -- not so useful info
                     callProcess "bash" ["-c","ltsinfo go-model.lts"]
                   --
                   --
                   when ((check pargs) ==  Terminal || (check pargs)==All) $ do
                     writeToFile "formula-ts.mcf" (noTerminalState ntaus)
                     parseModelChecker (debug pargs) True "go-model.lps" "formula-ts.mcf"
                       >>= (printResult "No terminal state:\t")
                   --
                   when ((check pargs) ==  Terminal || (check pargs)==All) $ do
                     writeToFile "formula-nocycle.mcf" (noCycle ntaus)
                     parseModelChecker (debug pargs) True "go-model.lps" "formula-nocycle.mcf"
                       >>= (printResult "No cycle:\t\t")
                   --
                   when ((check pargs) ==  Deadlock || (check pargs)==All) $ do
                     writeToFile "formula-df.mcf" (globalDeadlock ntaus barbs)
                     parseModelChecker (debug pargs) True "go-model.lps" "formula-df.mcf"
                       >>= (printResult "No global deadlock:\t")
                   --
                   when ((check pargs) ==  Liveness || (check pargs)==All || (check pargs)==Benchmarks) $ do
                     writeToFile "formula-live.mcf" (liveness Nothing ntaus barbs)
                     live <- parseModelChecker (debug pargs) True "go-model.lps" "formula-live.mcf"
                     printResult "Liveness:\t\t" live
                     when ((not live) && (chanlive pargs)) $ do
                       putStrLn $ "Checking liveness for "++(show $ length chans)++ " channels:"
                       mapM_ chanLiveness (map fst chans)

                   when ((check pargs) ==  Safety || (check pargs)==All || (check pargs)==Benchmarks) $ do
                     case safety ntaus barbs of
                       Just csf -> do writeToFile "formula-safety.mcf" csf
                                      parseModelChecker (debug pargs) True "go-model.lps" "formula-safety.mcf"
                                       >>= (printResult "Safety:\t\t\t")
                       Nothing -> printResult "Safety:\t\t\t" True

                   when ((check pargs) ==  DataRaceFree || (check pargs)==All || (check pargs)==Benchmarks) $ do
                     case drf ntaus barbs of
                       Just csf -> do writeToFile "formula-drf.mcf" csf
                                      parseModelChecker (debug pargs) True "go-model.lps" "formula-drf.mcf"
                                       >>= (printResult "Data race free:\t\t")
                       Nothing -> printResult "Data race free:\t\t" True


                   when ((check pargs)==All || (check pargs)==Eventual) $ do
                     writeToFile "formula-er.mcf" (eventualReception ntaus barbs)
                     parseModelChecker (debug pargs) True "go-model.lps" "formula-er.mcf"
                       >>= (printResult "Eventual reception:\t") 
                   --
--                   when ((check pargs) ==  Termination) $ do
--                           putStrLn $ "Termination Analysis:" ;
--                          if (null fors) then
--                             (printResult "\t No For loops, trivially " True)
--                           else
--                            do
--                            when (deb) $ do
--                                         putStrLn $ "C-Code: \n" ++ cCode ;
--                                        putStrLn $ "Main: \n" ++ m ;
--                                         putStrLn $ "Calls with Parameters: "++ (show l);
--                                         putStrLn $ "Calls in Main: "++(show mainCalls)
--                            writeCCode fname (cCode ++ m ++ "\n") ;
--                            runClang fname ;
--                            res <- runTermCheck fname l deb ; return ()
--                            when (not res) (runStepWiseCheck fname
--                                           mainCalls deb)
                     
                   
writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content



printmyprog :: ProgGo -> ProgGo -> IO ()
printmyprog (P xs) (P ys) = do putStr $
                                 "Number of definitions: "
                                 ++(show $ length xs)++"/"
                               putStrLn $ (show $ length ys)
                                 ++" before/after optimisation"

--
generateLPSFile :: Bool -> String -> String -> String -> IO ()
generateLPSFile debug model outfile strat =
  let mcmd = "mcrl22lps -l"++strat
             ++
             (if debug then " -v --timings " else " -q " )
             ++
             model++" > "++outfile
  in do
    out <- readProcess "bash" ["-c",mcmd] []
    return ()

generateLTSFile :: String -> String -> IO ()
generateLTSFile lpsfile outfile =
  let mcmd = "lps2lts -v "++lpsfile++" "++outfile
  in do out <- readProcess "bash" ["-c",mcmd] []
        return ()

--
parseModelChecker :: Bool -> Bool -> String -> String -> IO Bool
parseModelChecker debug bool model formula = 
  let mcf = if bool
            then formula
            else "/dev/stdin"
      pref = if bool
             then ""
             else "echo \""++formula++"\" | "
      mcmd = pref++"lps2pbes"
             ++
             (if debug then " --timings -vf " else " -qf ")
             ++
             mcf++" "++model++" | pbes2bool "
             ++
             (if debug then " --timings " else "")
             ++
             " -s1"
             ++
             (if debug then " -v" else " -q")
  in do out <- readProcess "bash" ["-c",mcmd] []
        return $ (unpack. strip . pack $ out)=="true"


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
     cmd = "llvm2kittel " ++ fun ++ bcfile ++ "> " ++ kfile in
     do
      out <- readProcess "bash" ["-c",cmd] []
      if (not . null $ out) then
        error ("Error Converting to Termination Analysis: "++out)
      else
        return ()

-- pre: fname is path/file.cgo
runKittel fname =
  let kfile = (dropExtension fname) <.> ".kittel"
      cmd = "kittel.native -timeout 15 " ++ kfile
  in
    do
      (code,out,err) <- readProcessWithExitCode "bash" ["-c",cmd] []
      case code of
         ExitSuccess -> return $ (((elem $ "Termination successfully shown!") . L.lines $ out),out,False)
         ExitFailure _ -> if (null err) then
                            return $ (False, head (L.lines out) ++ "(Likely non-terminating)",True)
                          else 
                            error err

-- pre: fname is path/file.cgo
-- l: list of functions to test explicitly
runTermCheck fname [] debug = do
    runLLVM2K fname "" ;
    (res,out,timeout) <- runKittel fname
    if (timeout) then printErr "\t Main: " out
                 else printResult "\t Main: " res 
    when (debug) (printResult ("\t"++out) res);
    return res
      
runTermCheck fname (x:xs) debug = do
    runLLVM2K fname x ;
    (res,out,timeout) <- runKittel fname
    if (timeout) then printErr ("\t Function " ++ x ++ ": ") out
                 else printResult ("\t Function " ++ x ++ ": ") res
    --printResult ("\t Function " ++ x ++ ": ") res ;
    when (debug) (printResult ("\t"++out) res) ;
    runTermCheck fname xs debug

runStepWiseCheck fname [] debug = return ()
runStepWiseCheck fname (x:xs) debug = do
    runLLVM2K fname x ;
    (res,out,timeout) <- runKittel fname
    if (timeout) then printErr ("\t Function " ++ x ++ ": ") out
                 else printResult ("\t Function " ++ x ++ ": ") res
    when (debug) (printResult ("\t"++out) res) ;
    runStepWiseCheck fname xs debug          


printHeader :: String -> IO()
printHeader s = do setSGR [SetConsoleIntensity BoldIntensity]
                   putStrLn s
                   setSGR [Reset]
                   hSetSGR stderr [Reset]
                   
printResult :: String -> Bool -> IO()
printResult s t = do putStr $ s
                     selectColor t
                     putStrLn $ show t
                     setSGR [Reset]
                     hSetSGR stderr [Reset]

printErr :: String -> String -> IO()
printErr s t = do putStr $ s
                  setSGR [SetColor Foreground Vivid Red]
                  putStrLn t
                  setSGR [Reset]
                  hSetSGR stderr [Reset]
                     
selectColor :: Bool -> IO()
selectColor True =  setSGR [SetColor Foreground Vivid Green]
selectColor False =  setSGR [SetColor Foreground Vivid Red]

