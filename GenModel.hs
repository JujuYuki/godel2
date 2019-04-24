module GenModel where

import GoParser 
import Mcrl
import FlatGo

import Data.List as L
import Data.Maybe
import qualified Data.Map as M
import Data.Char (toUpper, isUpper)



prefixesList :: [String]
prefixesList = ["Chan_Close_"          -- closing channel (from CHAN)
               , "Closed_Chan_Rcv_"    -- emit rcv/closed (from CHAN)
               , "Closed_Chan_Rcv_Select_"
               , "Pop_"                -- put data on chan (from ASYNC CHAN) 
               , "Push_"               -- get data from chan  (from ASYNC CHAN)
               , "Send_"               -- Normal send
               , "Send_Select_"
               , "Rcv_"                -- Normal receive
               , "Rcv_Select_"        
               , "Rcv_Close_"          
               , "Select_Send_"        -- Select actions
               , "Select_Rcv_"
               , "Select_NRcv_"
               , "Select_NSend_"
               , "Select_Rcv_Close_"
               , "Sync_"
               , "Close_"             -- closing primitive
               , "Closing_"
               , "Write_"             -- shared memory primitives
               , "Var_Write_"
               , "Read_"
               , "Var_Read_"
               , "Lock_"              -- mutexes primitives
               , "Mutex_Lock_"
               , "Unlock_"
               , "Mutex_Unlock_"
               , "RLock_"
               , "RWMutex_RLock_"
               , "RUnlock_"
               , "RWMutex_RUnlock_"
               , "LockWait_"
               , "RWMutex_LockWait_"
               ]


mkRelabelVar :: String -> [((String,String),String)]
mkRelabelVar var = map (\x -> (x,"Sync_"++var)) $ varlist
  where varlist = [ ("Read_"++var,"Var_Read_"++var)
                  , ("Write_"++var,"Var_Write_"++var)
                  ]
    
mkRelabelMut :: (String, String) -> [((String,String),String)]
mkRelabelMut (mut, t)
  | t == "mutex"   = map (\x -> (x,"Sync_"++mut)) $ common
  | t == "rwmutex" = map (\x -> (x,"Sync_"++mut)) $ common++rwlist
  where common = [ ("Lock_"++mut,"Mutex_Lock_"++mut)
                 , ("Unlock_"++mut,"Mutex_Unlock_"++mut)
                 ]
        rwlist = [ ("RLock_"++mut,"RWMutex_RLock_"++mut)
                 , ("RUnlock_"++mut,"RWMutex_RUnlock_"++mut)
                 , ("LockWait_"++mut,"RWMutex_LockWait_"++mut)
                 ]

mkRelabel :: (String, Int) -> [((String,String),String)]
mkRelabel (chan, i)
  | i == 0 = closing:(map (\x -> (x,"Sync_"++chan)) $ synclist++common)
  | i > 0  = closing:(map (\x -> (x,"Sync_"++chan)) $ asynclist++common)
  where common = [ ("Closed_Chan_Rcv_"++chan,"Rcv_Close_"++chan)
                 , ("Closed_Chan_Rcv_Select_"++chan,"Select_Rcv_Close_"++chan)
                 ]
        synclist = [ ("Select_NSend_"++chan,"Rcv_Select_"++chan)
                   , ("Select_Send_"++chan,"Select_Rcv_"++chan)
                   , ("Send_"++chan,"Rcv_"++chan)
                   , ("Send_Select_"++chan,"Select_NRcv_"++chan)
                   ]
        asynclist = [ ("Send_"++chan, "Push_"++chan)
                    , ("Select_Send_"++chan, "Push_Select_"++chan)
                    , ("Rcv_"++chan, "Pop_"++chan)
                    , ("Select_Rcv_"++chan, "Pop_Select_"++chan)
                    ]
        closing = (("Close_"++chan, "Chan_Close_"++chan), "Closing_"++chan)


type GuardedModel = (Maybe Model, Model)


mkSend :: String -> GuardedModel
mkSend chan = (Just $ A $ Barb (Send chan)
               , Plus (A $ Action "Send_" chan) (A $ Action "Send_Select_" chan))
              
mkClose :: String -> GuardedModel
mkClose chan = (Just $ A $ Barb (Closing chan), A $ Action "Close_" chan)


mkChanClosedSend :: String -> Model
mkChanClosedSend chan =  Plus
                         (A $ Action "Closed_Chan_Rcv_" chan)
                         (A $ Action "Closed_Chan_Rcv_Select_" chan)
                        

mkVarRW :: String -> Model
mkVarRW var = Plus
              (A $ Action "Var_Read_" var)
              (A $ Action "Var_Write_" var)


mkRcv :: String -> GuardedModel
mkRcv chan = (Just $ A $ Barb (Rcv chan)
              , Plus (A $ Action "Rcv_" chan)
                (Plus
                 (A $ Action "Rcv_Select_" chan)
                 (A $ Action "Rcv_Close_" chan)
                ))

mkSelectSend :: String -> Model
mkSelectSend chan =  Plus
                     (A $ Action "Select_Send_" chan)
                     (A $ Action "Select_NSend_" chan)
                   
                   

mkSelectRcv :: String -> Model
mkSelectRcv chan =  Plus (A $ Action "Select_Rcv_" chan)
                    (Plus
                     (A $ Action "Select_NRcv_" chan)
                     (A $ Action "Select_Rcv_Close_" chan)
                    )
                   

mkRead :: String -> GuardedModel
mkRead var = (Just $ A $ Barb (Read var), A $ Action "Read_" var)

mkWrite :: String -> GuardedModel
mkWrite var = (Just $ A $ Barb (Write var), A $ Action "Write_" var)

mkLock :: String -> GuardedModel
mkLock mut = (Just $ A $ Barb (Lock mut), Plus
              (A $ Action "Lock_" mut)
              (Dot (A $ Action "LockWait_" mut)
               (Plus (A $ Barb (Lock mut))
                (A $ Action "Lock_" mut))))

mkRLock :: String -> GuardedModel
mkRLock mut = (Just $ A $ Barb (RLock mut), A $ Action "RLock_" mut)

mkUnlock :: String -> GuardedModel
mkUnlock mut = (Just $ A $ Barb (Unlock mut), A $ Action "Unlock_" mut)

mkRUnlock :: String -> GuardedModel
mkRUnlock mut = (Just $ A $ Barb (RUnlock mut), A $ Action "RUnlock_" mut)

mkProcName :: String -> String
mkProcName xs = concat $ map fun xs
  where fun x = if isUpper x
                then ['_', toUpper x]
                else [toUpper x]
                

genBody :: Interm String -> GuardedModel
genBody (Call fun xs) = (Nothing, Rec $ mkProcName fun)
genBody (Spawn fun xs) = (Nothing, Rec $ mkProcName fun)
genBody (Seq xs) = (Nothing, genSequencing xs)
genBody (If e t t') = (Nothing, Plus
                                (Dot Tau (unGuardModel $ genBody t))
                                (Dot Tau (unGuardModel $ genBody t'))
                      )
genBody (IfCond _ _ _ t t') = (Nothing, Plus
                                        (Dot Tau (unGuardModel $ genBody t))
                                        (Dot Tau (unGuardModel $ genBody t'))
                              )
genBody (Select xs) = (Nothing, genSelect xs)
genBody (S chan) = mkSend chan
genBody (R chan) = mkRcv chan
genBody (Ld var) = mkRead var
genBody (St var) = mkWrite var
genBody (Lck mut) = mkLock mut
genBody (Ulck mut) = mkUnlock mut
genBody (RLck mut) = mkRLock mut
genBody (RUlck mut) = mkRUnlock mut
genBody T = (Nothing, Tau)
genBody Zero = (Nothing, Delta)
genBody (Cl chan) = mkClose chan
genBody t = error $ show t


unGuardModel :: GuardedModel -> Model
unGuardModel (Just m1, m2) = Plus (Dot m1 Delta) m2
unGuardModel (Nothing, m2) = m2



genSequencing :: [Interm String] -> Model
genSequencing xs = helper xs
  where helper [] = Delta
        helper [x] = unGuardModel $ genBody x
        helper (Zero:xs) = helper xs
        helper (x:xs) = case x of
          (Spawn n ys) -> Dot (A $ Request $ mkProcName n) (helper xs)
          t -> let (m1, m2) = genBody x
               in case m1 of
                 Nothing -> Dot m2 (helper xs)
                 Just m' -> Plus m' (Dot m2 (helper xs))



genSelect :: [Interm String] -> Model
genSelect xs = Plus (Dot (mkSelectBarb xs) Delta) (helper xs)
  where helper [x] = genSelectBranch x
        helper (x:ys) = Plus (genSelectBranch x) (helper ys)


mkSelectBarb :: [Interm String] -> Model
mkSelectBarb xs = let guards = sort $ nub $ map selectGuard xs
                  in if T `elem` guards
                     then A $ Barb $ Sel []
                     else A $ Barb $ Sel $ map
                          (\x -> case x of
                              (R chan) -> Rcv chan
                              (S chan) -> Send chan
                          ) guards

selectGuard :: Interm String -> Interm String
selectGuard (Seq (x:xs)) = selectGuard x
selectGuard (S chan) = S chan
selectGuard (R chan) = R chan
selectGuard T = T
selectGuard t = error $ "[selectGuard] No guard in select: "++(show t)++"."

genSelectBranch :: Interm String -> Model
genSelectBranch (Seq (x:xs)) = Dot (genSelectBranch x) (unGuardModel $ genBody (Seq xs))
genSelectBranch (S chan) = mkSelectSend chan
genSelectBranch (R chan) = mkSelectRcv chan
genSelectBranch T = Tau
genSelectBranch t = error $ "[genSelectBranch] No guard in select: "++(show t)++"."



countSpawnees :: Interm String -> M.Map String Int
countSpawnees t = helper t
  where helper (Spawn s xs) = M.singleton s 1
        helper (If e t t') = M.unionWith (\x y -> maximum [x,y]) (helper t) (helper t')
        helper (IfCond _ _ _ t t') = M.unionWith (\x y -> maximum [x,y]) (helper t) (helper t')
        helper (Select xs) = M.unionsWith (\x y -> maximum [x,y]) $ map (helper) xs
        helper (Seq xs) = M.unionsWith (+) $ map (helper) xs
        helper t = M.empty
        

genProgram :: ProgGo -> (String, [(String, Int)], [String], [(String, BarbContent)])
genProgram (P xs@((D t0 args t):ys)) =
  let model = "act\n"
              ++
              "tausync; \n"
              ++
              "% actions\n"
              ++
              (if not $ null declaredactions
               then (intercalate ", " declaredactions)++";\n"
               else "")
              ++
              "% barbs\n" 
              ++
              (if not $ null allguards
               then (intercalate ", " $ map printBarbs allguards)++";\n"
               else "")
              ++
              "% spawnings\n" 
              ++
              (if not $ null spawneelist
               then (intercalate ", " $
                     foldl (++) [] $
                     map (\(f,m) -> ["REQ_"++f,"ACC_"++f]) spawneelist)++";\n"
               else ""
              )
              ++
              "\nproc\n"
              ++
              (intercalate ";\n" $ map (\(f,m) -> f++" = "++(printModel (f, m))) eqlist)++";\n\n"
              --
              ++
              (if null eqchanlist
               then ""
               else (intercalate ";\n\n" $ map (\(f,m) -> f++" = "++printModel (f, m)) eqchanlist)++";\n\n")
              ++
              (if null eqvarlist
               then ""
               else (intercalate ";\n\n" $ map (\(f,m) -> f++" = "++printModel (f, m)) eqvarlist)++";\n\n")
              ++
              (if null eqmutlist
               then ""
               else (intercalate ";\n\n" $ map (\(f,m) -> f++" = "++printModel (f, m)) eqmutlist)++";\n\n")
              ++
              "CHANS = "
              ++(if null allchans
                 then "delta; \n"
                 else (intercalate " || " $ map initChan allchans)++";\n")
              ++
              "VARS = "
              ++(if null allvars
                 then "delta; \n"
                 else (intercalate " || " $ map initVar allvars)++";\n")
              ++
              "SYNC = "
              ++(if null allmuts
                 then "delta; \n"
                 else (intercalate " || " $ map initMut allmuts)++";\n")
              ++
              "\ninit\n"
              ++
              "allow({"++(intercalate ", " $ nub $ (map printBarbs allguards)++taus++["tausync"])
              ++
              "},\n"
              ++
              (if (null commactions) && (null spawneelist)
               then "("
               else "comm({"++(intercalate ",\n" $
                               map (\((s,r),t) -> s++"|"++r++"->"++t) commactions)++"\n"
                    ++
                    (if not $ null spawneelist
                     then (if (null commactions)
                           then ""
                           else ", ")
                           ++(intercalate ", " $
                                 map (\(f,m) -> "REQ_"++f++"|ACC_"++f++"->tausync") spawneelist)++"\n"
                     else ""
                    )
              ++"},"
              )
              ++
              "\n CHANS || VARS || SYNC || "++(mkProcName t0)++printSpawnee++"));"
      taus = map snd commactions
  in (model, allchans, taus, allguards)
                    
  where allchans = allChans (P xs)
        allvars = allVars (P xs)
        allmuts = allMuts (P xs)
        --
        golist = map (\(D fun args t) -> (mkProcName fun, cleanUp isNewChanVar t)) xs
        eqlist = map (\(x,y) -> (x, unGuardModel $ genBody y)) golist
        --
        spawnees = M.mapKeys mkProcName $
                   M.unionsWith (+) $ map (\(f,t) -> countSpawnees t) golist
        spawneelist = nub $ catMaybes $ map (\(x,y) -> case M.lookup x spawnees of
                                              Just i -> Just (x,y)
                                              Nothing -> Nothing) eqlist
        --
        eqchanlist = foldr (++) [] $ map genChan allchans
        eqvarlist = foldr (++) [] $ map genVar allvars
        eqmutlist = foldr (++) [] $ map genMut allmuts
        allguards = nub $ foldr (++) [] $ map (getGuards) (eqlist++eqchanlist++eqvarlist++eqmutlist)
        allactions = nub $ foldr (++) [] $ map (getActions . snd) (eqlist++eqchanlist)
        commactions = filter (\((s,r),t) -> s `elem` allactions || r `elem` allactions) $
                      foldr (++) [] $
                      ((map mkRelabel allchans)++(map mkRelabelVar allvars)++(map mkRelabelMut allmuts))
        declaredactions = nub $ (foldr (++) [] $ map (\((x,y),z) -> [x,y,z]) commactions)++allactions
        --
        printSpawnee = if not $ M.null spawnees
                       then "||" ++ (intercalate " || " $
                                     foldl (++) [] $
                                     map (\(n,i) -> replicate i  ("(ACC_"++n++" . "++n++")")) $
                                     M.toList spawnees)
                       else ""

isZero :: Interm String -> Bool
isZero (Zero) = True
isZero _ = False

isNewChanVar :: Interm String -> Bool
isNewChanVar (NewChan chan s sy) = True
isNewChanVar (NewVar var) = True
isNewChanVar (NewSync mut t) = True
isNewChanVar _ = False


unPackSeq :: Interm String -> Interm String
unPackSeq (Seq [x]) =  unPackSeq x
unPackSeq (Seq xs) = Seq $ map unPackSeq xs
unPackSeq t  = t

cleanUp :: (Interm String -> Bool) -> Interm String ->  Interm String
cleanUp f (Seq [x]) = cleanUp f (unPackSeq x)
cleanUp f (Seq zs) = case (filter (not . f) $ map (cleanUp f) zs) of
  [] -> Zero
  ys -> case map unPackSeq ys of
    [] -> Zero
    [x] -> x
    (x:xs) -> Seq (x:xs)
cleanUp f (Select xs) = Select $ filter (not . f) $ map (cleanUp f) xs
cleanUp f (If e t t') = If e (cleanUp f t) (cleanUp f t')
cleanUp f (IfCond e1 e2 e3 t t') = IfCond e1 e2 e3 (cleanUp f t) (cleanUp f t')
cleanUp f t 
  | f t = Zero
  | otherwise = t



cleanUpProg :: ProgGo -> ProgGo
cleanUpProg (P xs) =  P $ map (\(D fun args t) -> D fun args (cleanUp isZero t)) xs

collectChans :: Interm String -> [(String, Int)]
collectChans (NewChan chan s sy) = [(chan,fromIntegral sy)]
collectChans (Seq xs) = foldr (++) [] $ map collectChans xs
collectChans (If e t t') = (collectChans t)++(collectChans t')
collectChans (IfCond _ _ _ t t') = (collectChans t)++(collectChans t')
collectChans (Select xs) = foldr (++) [] $ map collectChans xs
collectChans t = []

collectVars :: Interm String -> [String]
collectVars (NewVar var) = [var]
collectVars (Seq xs) = foldr (++) [] $ map collectVars xs
collectVars (If e t t') = (collectVars t)++(collectVars t')
collectVars (IfCond _ _ _ t t') = (collectVars t)++(collectVars t')
collectVars (Select xs) = foldr (++) [] $ map collectVars xs
collectVars t = []

collectMuts :: Interm String -> [(String, String)]
collectMuts (NewSync mut t) = [(mut, t)]
collectMuts (Seq xs) = foldr (++) [] $ map collectMuts xs
collectMuts (If e t t') = (collectMuts t)++(collectMuts t')
collectMuts (IfCond _ _ _ t t') = (collectMuts t)++(collectMuts t')
collectMuts (Select xs) = foldr (++) [] $ map collectMuts xs
collectMuts t = []

allChans :: ProgGo -> [(String, Int)]
allChans (P xs) = nub $ foldr (++) [] $ map (\(D fun args t) -> collectChans t) xs

allVars :: ProgGo -> [String]
allVars (P xs) = nub $ foldr (++) [] $ map (\(D fun args t) -> collectVars t) xs

allMuts :: ProgGo -> [(String, String)]
allMuts (P xs) = nub $ foldr (++) [] $ map (\(D fun args t) -> collectMuts t) xs


declareChan :: (String, Int) -> String
declareChan (chan, n) = intercalate ", " $ map (\x -> x++chan) prefixesList 

declareVar :: String -> String
declareVar var = intercalate ", " $ map (\x -> x++var) prefixesList

declareMut :: (String, String) -> String
declareMut (mut, t) = intercalate ", " $ map (\x -> x++mut) prefixesList
                        
initChan :: (String, Int) -> String
initChan (chan, n)
  | n == 0 = mkProcName chan
  | n > 0 = (mkProcName chan)++"("++(show n)++",0)"

initVar :: String -> String
initVar var = mkProcName var

initMut :: (String, String) -> String
initMut (mut, t)
  | t == "mutex" = mkProcName mut
  | t == "rwmutex" = (mkProcName mut)++"(0)"

genVar :: String -> [(String, Model)]
genVar var = let f1 = mkProcName var
                 t1 = Dot (mkVarRW var)
                       (Rec $ mkProcName var)
             in [(f1,t1)]

genMut :: (String, String) -> [(String, Model)]
genMut (mut, t)
  | t == "mutex" = let f1 = mkProcName mut
                       t1 = Dot (A $ Action "Mutex_Lock_" mut)
                             (Rec $ "LOCKED_"++(mkProcName mut))
                       f2 = "LOCKED_"++(mkProcName mut)
                       t2 = Plus (A $ Barb (Locked mut))
                            (Dot (A $ Action "Mutex_Unlock_" mut)
                             (Rec $ mkProcName mut)
                            )
                   in [(f1,t1),(f2,t2)] 
  | t == "rwmutex" = let f1 = (mkProcName mut)++"(n: Int)"
                         t1 =
                           Choice
                           [ ("", (Plus
                                   (Dot (A $ Action "RWMutex_LockWait_" mut)
                                    (Rec $ "LOCKWAIT_"++(mkProcName mut)++"(n)")
                                   )
                                   (Dot (A $ Action "RWMutex_RLock_" mut)
                                    (Rec $ (mkProcName mut)++"(n+1)")
                                   )
                                  )
                             )
                           , ("n>0", (Plus (A $ Barb (RLocked mut))
                                      (Dot (A $ Action "RWMutex_RUnlock_" mut)
                                       (Rec $ (mkProcName mut)++"(n-1)")
                                      )
                                     )
                             )
                           ]
                         f2 = "LOCKED_"++(mkProcName mut)
                         t2 = Plus (A $ Barb (Locked mut))
                            (Dot (A $ Action "Mutex_Unlock_" mut)
                             (Rec $ (mkProcName mut)++"(0)"))
                         f3 = "LOCKWAIT_"++(mkProcName mut)++"(n: Int)"
                         t3 =
                           Choice
                           [ ("n==0", (Dot (A $ Action "Mutex_Lock_" mut)
                                       (Rec $ "LOCKED_"++(mkProcName mut))
                                      )
                             )
                           , ("n>0", (Plus (A $ Barb (RLocked mut))
                                      (Dot (A $ Action "RWMutex_RUnlock_" mut)
                                        (Rec $ "LOCKWAIT_"++(mkProcName mut)++"(n-1)"))
                                     )
                             )
                           ]
                     in [(f1,t1),(f2,t2),(f3,t3)]

genChan :: (String, Int) -> [(String, Model)]
genChan (chan, i)
  | i == 0 = let f1 = mkProcName chan
                 t1 = Dot
                      (A $ Action "Chan_Close_" chan)
                      (Rec $ "CLOSED_"++(mkProcName chan))
                 f2 = "CLOSED_"++(mkProcName chan)
                 t2 = Plus (A $ Barb (Closed chan)) 
                     ( Dot (mkChanClosedSend chan)
                       (Rec $ "CLOSED_"++(mkProcName chan))
                     )
             in [(f1,t1),(f2,t2)]
               
  | i > 0 = let f1 = (mkProcName chan)++"(n: Nat, k: Int)"
                t1 =
                  Choice
                  [ ("", (Dot
                         (A $ Action "Chan_Close_" chan)
                         (Rec $ "CLOSED_"++(mkProcName chan)))
                    )
                  , ("k>0", Dot
                            (Plus (A $ Action "Pop_" chan)
                             (Plus (A $ Action "Pop_Select_" chan)
                              (A $ Barb $ NonEmpty chan)
                             )
                            )
                            (Rec $ (mkProcName chan)++"(n,k-1)")
                    )
                  , ("k<n", Dot
                            (Plus (A $ Action "Push_" chan) (A $ Action "Push_Select_" chan))
                            (Rec $ (mkProcName chan)++"(n,k+1)")
                    )
                  ]
                f2 = "CLOSED_"++(mkProcName chan)
                t2 = Plus (A $ Barb (Closed chan)) 
                     ( Dot (mkChanClosedSend chan)
                       (Rec $ "CLOSED_"++(mkProcName chan))
                     )
            in [(f1,t1),(f2,t2)]


