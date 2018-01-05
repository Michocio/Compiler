module IntermediateCode where


import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Control.Monad.Except
import qualified Data.Map as M
import Data.Char
import LexGrammar
import ParGrammar
import SkelGrammar
import PrintGrammar
import AbsGrammar
import ErrM
import Data.Typeable
import Control.Exception
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Data.List
import Data.Maybe
import System.Exit ( exitFailure, exitSuccess )
import Data.Tuple
import Data.Function (on)


import Environment
import TypesStuff
import IntermediateEnv
import Misc
import Optimisations

type SimpleBlocks = M.Map Argument [Tuple]
type ControlFlow = (Argument, [Argument])


generateIntermediate :: ProgramD -> IO ()
generateIntermediate p =  (liftIO $ evalStateT (genProgram p) initialEnvMid)

genProgram :: ProgramD -> StateT EnvMid IO ()
genProgram (Program info []) = return ()
genProgram  (Program info (x:xs)) = do
    --lEntry <- genLabel "entry"
    genTopDef (x:xs)

genTopDef :: [TopDefD] -> StateT EnvMid  IO ()
genTopDef [] = return ()
genTopDef  (x:xs) = do
    genTopInstr x
    genTopDef xs

genBlocks :: [(Argument, (Int, Int))] -> [Tuple] -> SimpleBlocks -> SimpleBlocks
genBlocks [] _ graph = graph
genBlocks ((arg, (start, end)):xs) code graph =
    genBlocks xs (drop (end - start + 1) code) (M.insert arg (take (end - start + 1) code) graph)

returnJump :: Int -> [Int] -> [Tuple] -> [Int]
returnJump _ old [] = old
--returnJump x old (((IfOp how), a1, a2, (Label _ pos)):xs) = returnJump (x+1) (x:old) xs
--returnJump x old ((GotoOp, (Label _ pos), _, _):xs) = returnJump (x+1) (x:old) xs
--returnJump x old ((RetOp, _, _, _):xs) = returnJump (x+1) (x:old) xs
returnJump y old (x:xs) = returnJump (y+1) old xs

genTopInstr :: TopDefD -> StateT EnvMid IO ()
genTopInstr fun@(FnDef a type_ ident@(Ident name) args (Block t block)) = do
    put initialEnvMid
    mapM genStmt block
    liftIO $ putStrLn $ "##########" ++ (show name) ++ ": "
    printCode
    return ()

removeDeadCode :: [Tuple] -> [Tuple]
removeDeadCode code =
    case (findIndex dropAfterRet code) of
        (Nothing) -> code
        (Just i) -> take i code


printCode :: StateT EnvMid IO ()
printCode = do
    emit(EmptyOp, NIL, NIL, NIL)
    putEntryLabel
    --sortedLabels <- showCode
    removeDupLabels
    sortedLabels <- showCode
    (blocks, graph) <- createBlockGraph sortedLabels
    codes <- (return $ map snd (M.toList blocks))
    vs <- return $ map usedVars (map snd (M.toList blocks))
    liftIO $ putStrLn $ show vs
    news <-(mapM addPhi (zip vs codes))
    liftIO $ mapM (mapM (print. printTuple)) news
    --edited <- doOpt (map snd (M.toList blocks))
    --edited <- mapM constFolding (map snd (M.toList blocks))
    {-liftIO $ putStrLn "xxxx"
    liftIO $ mapM (mapM (print. printTuple)) edited
    (a,b) <- return $ aliveVar ([],[]) (foldr (++) [] edited)
    k <- return $ map (map (removeDeadUsage b)) edited
    z <- return $ map ((filter isJust)) k
    zz <- return $ map (map fromJust) z
    --yy <- return $ map removeDeadCode zz
    liftIO $ putStrLn "YYYYYYYYYYYYYYYYYYYYYYYYYY"
    liftIO $ mapM (mapM (print. printTuple)) zz
    liftIO $putStrLn $ show zz
    liftIO $putStrLn $ show sortedLabels
    liftIO $putStrLn $ show graph-}
    return ()

addPhi :: ([String], [Tuple]) -> StateT EnvMid IO [Tuple]
addPhi (vars, code) = do
    x <- (mapM genPhi vars)
    y <- return $ x ++ code
    return y

genPhi :: String -> StateT EnvMid IO Tuple
genPhi name = do
     x <- assingVar name
     return (Phi, x, From 0 (Var "x" 0 1), From 0 (Var "y" 0 1))

usedVars :: [Tuple] -> [String]
usedVars code =
    nub $ foldr (++) [] (map allVars code)

allVars :: Tuple -> [String]
allVars (op, a1, a2, a3) = (ifVar a1) ++ (ifVar a2)

ifVar :: Argument -> [String]
ifVar (Var name x y) = [name]
ifVar x = []

createBlockGraph :: [((String, Int), Int)] -> StateT EnvMid IO (M.Map Int [Tuple], (M.Map Int [Argument]))
createBlockGraph sorted_blocks = do
    labels <- getLabels
    code <- getCode
    extractedBlocks <- return $ map (\(k,(l, pos)) -> (k, pos)) (M.toList labels)
    starts <- return $ sortBy (compare `on` (snd)) extractedBlocks
    (_,_,_,_, res) <- return $ genBlock (fst $ head starts, 0, code, tail starts, M.empty)
    ord <- return $ map fst $ map swap sorted_blocks
    sorted <- return $ sortMap ord res []
    acc <- genControlFlow sorted ord M.empty
    wynik <- traverseBlocks [] acc res M.empty (Label "l" (head ord))
    return (wynik, acc )

removeDupLabels :: StateT EnvMid IO ()
removeDupLabels = do
    labels <- getLabels
    code <- getCode
    same <- return $ map (\(nr, (_, code))-> (code, nr)) (M.toList labels)
    same_code_ <- return $ groupLabels same M.empty
    same_code <- return $ map snd (M.toList same_code_)
    mapM alterLabels same_code
    mapM eraseDupsLabels (map tail same_code)
    return ()

groupLabels :: [(Int, Int)] -> M.Map Int [Int] -> M.Map Int [Int]
groupLabels [] old = old
groupLabels ((c, nr):xs) old =
    case (M.lookup c old) of
        Nothing -> groupLabels xs (M.insert c [nr] old)
        (Just x) ->  groupLabels xs (M.insert c (x++[nr]) old)

eraseDupsLabels :: [Int] -> StateT EnvMid IO ()
eraseDupsLabels ls = do
    mapM deleteLabel ls
    return ()

alterLabels :: [Int] -> StateT EnvMid IO ()
alterLabels same = do
    if(length same == 1) then return ()
    else do
        leader <- return $ Label "l" (head same)
        changers <- return $ map (\x -> (Label "l" x)) (tail same)
        (vars, decls, temps, lab, code, labels, curr) <- get
        diff <- return $ map (changeLabel leader changers) code
        put(vars, decls, temps, lab, diff, labels, curr)
        return ()

changeLabel ::  Argument -> [Argument] -> Tuple -> Tuple
changeLabel  leader cands (IfOp a, b, c, y) = do
    if(y `elem` cands) then (IfOp a, b, c, leader)
    else (IfOp a, b, c, y)
changeLabel leader cands (GotoOp, y, a, b) = do
    if(y `elem` cands) then (GotoOp, leader, a, b)
    else (GotoOp, y, a, b)
changeLabel aleader cands a = a

traverseBlocks :: [Int] -> M.Map Int [Argument] -> M.Map Int [Tuple] -> M.Map Int [Tuple] ->
    Argument -> StateT EnvMid IO (M.Map Int [Tuple])
traverseBlocks visited graph orginal old (Label _ node) = do
    if(node `elem` visited) then return old
    else do
        me <- return $ fromJust $ M.lookup node orginal
        ch <- return $ fromJust $ M.lookup node graph
        children <-  mapM (traverseBlocks (node:visited) graph orginal (M.insert node me old)) ch
        return $ foldr (M.union) (M.insert node me old) children

sortMap :: [Int] -> M.Map Int [Tuple] -> [(Int, [Tuple])] ->  [(Int, [Tuple])]
sortMap [] map_ list_ =  list_
sortMap (x:xs) map_ list_ =
    if (isJust $ M.lookup x map_) then sortMap xs map_ (list_ ++ ([(x,fromJust $ M.lookup x map_)]))
    else sortMap xs map_ list_

genControlFlow :: [(Int, [Tuple])] -> [Int] -> M.Map Int [Argument] -> StateT EnvMid IO (M.Map Int [Argument])
genControlFlow [] _ x = return x
genControlFlow  ((nr, []):xs) (y:ys) graph = do
    if(length xs == 0) then return graph
    else genControlFlow xs ys (M.insert nr [Label "l" (head ys)] graph)
genControlFlow ((nr, code):xs) (y:ys) graph = do
    instr <- return $ last code
    case (instr) of
        (IfOp a, _, _, jmp) -> do
            if(length xs == 0) then
                genControlFlow xs  ys (M.insert nr [jmp] graph)
            else
                genControlFlow xs ys(M.insert nr ([Label "l" (head ys)]++ [jmp]) graph)
        (RetOp, _, _, _) -> do
            updated <- return $ M.insert nr [] graph
            genControlFlow xs  ys updated
        (GotoOp, jmp, _, _) -> do
            liftIO $ putStrLn "JEEEEEEEEEEESTE"
            updated <- return $ M.insert nr [jmp] graph
            genControlFlow xs (ys) updated
        (otherwise) -> do
            if(length xs == 0) then
                genControlFlow xs ys (M.insert nr [] graph)
            else
                genControlFlow xs ys (M.insert nr ([Label "l" (head ys)]) graph)



dropAfterRet :: Tuple -> Bool
dropAfterRet (RetOp, _, _, _) = True
dropAfterRet _ = False

--index code labels old_map new labels new_map new index
genBlock :: (Int, Int, [Tuple], [(Int, Int)], M.Map Int [Tuple]) ->
    (Int, Int, [Tuple], [(Int, Int)], M.Map Int [Tuple])
genBlock (curr, index, [], [], blocks) = (curr, index, [], [], blocks)
genBlock (curr, index, code, [], blocks) = do
    case (M.lookup curr blocks) of
        Nothing -> (curr, index , [], [], M.insert curr code blocks)
        (Just x) -> (curr, index, [],[], M.insert curr (x++code) blocks)
genBlock (curr, index, [], labels, blocks) = (curr, index, [], labels, blocks)
genBlock (curr, index, (c:cx), ((nr, l):lx), blocks) = do
    if(index == l) then genBlock (nr, index, (c:cx), lx, blocks)
    else
        case (M.lookup curr blocks) of
            Nothing -> genBlock(curr, index + 1, cx, ((nr, l):lx), M.insert curr [c] blocks)
            (Just x) -> genBlock(curr, index + 1, cx, ((nr, l):lx), M.insert curr (x++[c]) blocks)


genItem :: Type (Maybe (Int, Int)) -> Item (Maybe (Int, Int)) -> StateT EnvMid IO ()
genItem  t (Init info (Ident name) expr) = do
    var <- newVar name
    res <- genExpr expr
    emit(Alloca t, var, NIL, NIL)
    emit(AssOp, res, var, NIL)
genItem t (NoInit info (Ident name)) =  do
    var <- newVar name
    emit(Alloca t, var, NIL, NIL)
    return ()

genStmt ::Stmt (Maybe(Int, Int)) -> StateT EnvMid IO ()
genStmt (Decl _ type_ items_) = do
    x <- mapM (genItem type_) items_
    return ()
genStmt  (BStmt x (Block a stmts)) = do
    (vars, decls, temps, labs, code, labels, curr) <- get
    put(M.union decls vars, decls, temps, labs, code, labels, curr)
    mapM genStmt stmts
    (_, _, temps_, labs_, code_, labels_, _) <- get
    put(vars, decls, temps_, labs_, code_, labels_, curr)
    return ()
genStmt  (SExp a expr) = do
    genExpr expr
    return ()
genStmt  (Ret info expr) = do
    ret <- genExpr expr
    emit(RetOp, ret, NIL, NIL)
genStmt  (VRet info) = emit(RetOp, NIL, NIL, NIL)
genStmt (Ass x lvalue@(ValArr a (Ident name) e) expr) = do
    val <- genExpr expr
    index <- genExpr e
    t <- freshTemp
    emit(GetElemPtr, Var name 0 0, index, t)
    emit(Store, val, t, NIL)
genStmt (Ass x var@(ValVar a name) expr) = do
    res <- genExpr expr
    ident <- assingVar $ snd $ stringLValue var
    emit(AssOp, res, ident, NIL)
    --emit(Store, res, Var (snd $ stringLValue var) 0 0, NIL)
genStmt (Incr a val) = do
    --temp <- freshTemp
    --emit(Load, Var (snd $ stringLValue val) 0, temp, NIL)
    --t <- freshTemp
    old <-  getVar (snd $ stringLValue val)
    var <-  assingVar (snd $ stringLValue val)
    emit(AddOp, old, ValInt 1, var)
    --emit(Store, t, Var (snd $ stringLValue val) 0, NIL)
genStmt (Decr a val) = do
    old <-  getVar (snd $ stringLValue val)
    var <-  assingVar (snd $ stringLValue val)
    emit(SubOp, old, ValInt 1, var)
    --temp <- freshTemp
    --emit(Load, Var (snd $ stringLValue val) 0, temp, NIL)
    --t <- freshTemp
    --emit(SubOp, temp, ValInt 1, t)
    --emit(Store, t, Var (snd $ stringLValue val) 0, NIL)
genStmt  (Empty a) = return ()
genStmt  (Cond info cond ifBlock) = do
    case (cond) of
        (ELitTrue a) -> do
            lTrue_ <- genLabel "l"
            genStmt (BStmt Nothing $ Block Nothing [ifBlock])
        (ELitFalse a) -> return ()
        otherwise -> do
            lTrue_ <- reserveLabel "lt"
            lTrue <- return $ Label  "lt" lTrue_
            lEnd_ <- reserveLabel "le"
            lEnd <- return $ Label  "le" lEnd_
            genCond cond lTrue lEnd lTrue
            updateLabel lTrue_
            liftIO $ putStrLn $ show ifBlock
            genStmt (BStmt Nothing $ Block Nothing [ifBlock])
            --emit(GotoOp, lEnd, NIL, NIL)
            updateLabel lEnd_
genStmt (CondElse info cond ifBlock elseBlock) = do
    case (cond) of
        (ELitTrue a) -> do
            lTrue_ <- genLabel "l"
            genStmt (BStmt Nothing $ Block Nothing [ifBlock])
        (ELitFalse a) -> do
            lFalse_ <-  genLabel "l"
            genStmt (BStmt Nothing $ Block Nothing [elseBlock])
        otherwise -> do
            lTrue_ <- reserveLabel "l"
            lTrue <- return $ Label  "l" lTrue_
            lFalse_ <- reserveLabel "l"
            lFalse <- return $ Label  "l" lFalse_
            lEnd_ <- reserveLabel "l"
            lEnd <- return $ Label  "l" lEnd_
            genCond cond lTrue lFalse lTrue
            updateLabel lTrue_
            genStmt (BStmt Nothing $ Block Nothing [ifBlock])
            emit(GotoOp, lEnd, NIL, NIL)
            updateLabel lFalse_
            genStmt (BStmt Nothing $ Block Nothing [elseBlock])
            updateLabel lEnd_
genStmt (While a expr stmt) = do
    case (expr) of
        (ELitTrue a) -> do
            lTrue_ <- genLabel "l"
            lEnd_ <- reserveLabel "l"
            lEnd <- return $ Label  "l" lEnd_
            genStmt (BStmt Nothing $ Block Nothing [stmt])
            emit(GotoOp, Label "l" lTrue_, NIL, NIL)
            updateLabel lEnd_
        (ELitFalse a) -> return ()
        otherwise -> do
            l1_ <- reserveLabel "l"
            l1 <- return $ Label  "l" l1_
            l2_ <- reserveLabel "l"
            l2 <- return $ Label  "l" l2_
            lEnd_ <- reserveLabel "l"
            lEnd <- return $ Label  "l" lEnd_
            emit(GotoOp, l2, NIL, NIL)
            updateLabel l1_
            genStmt (BStmt Nothing $ Block Nothing [stmt])
            updateLabel l2_
            genCond expr l1 lEnd lEnd
            updateLabel lEnd_

genCond :: Expr (Maybe (Int, Int)) -> Argument -> Argument -> Argument -> StateT EnvMid IO ()
genCond (ELitTrue a) lTrue lFalse lNext =
    emit(GotoOp, lTrue, NIL, NIL)
genCond (ELitFalse a) lTrue lFalse lNext = --do
    emit(GotoOp, lFalse, NIL, NIL)
genCond (EAnd _ e1 e2) lTrue lFalse lNext = do
    lMid_ <- (reserveLabel "l")
    lMid <- return $ Label  "l" lMid_
    genCond e1 lMid lFalse lMid
    updateLabel lMid_
    genCond e2 lTrue lFalse lNext
genCond (EOr _ e1 e2) lTrue lFalse lNext = do
    lMid_ <- (reserveLabel "l")
    lMid <- return $ Label  "l" lMid_
    genCond e1 lTrue lMid lMid
    updateLabel lMid_
    genCond e2 lTrue lFalse lTrue

genCond (Not _ e) lTrue lFalse lNext = genCond e lFalse lTrue lNext
genCond (ERel a expr1 relop_ expr2 ) lThen lElse lNext = do
    e1 <- genExpr expr1
    e2 <- genExpr expr2
    if(lNext == lThen) then do
        relop <- return $ compl relop_
        jmp <- return  lElse
        emitCond relop e1 e2 jmp
    else do
        if (lNext == lElse) then do
            relop <- return relop_
            jmp <- return  lThen
            emitCond relop e1 e2 jmp
        else do
            relop <- return  relop_
            jmp <- return  lThen
            emitCond relop e1 e2 jmp
            emit(GotoOp, lElse, NIL, NIL)
genCond e lThen lElse lNext = genCond (ERel Nothing e (EQU Nothing) (ELitTrue Nothing)) lThen lElse lNext

emitCond :: RelOp a-> Argument-> Argument-> Argument -> StateT EnvMid IO ()
emitCond relop e1 e2 jmp = do
    case (relop) of
        LTH x -> emit(IfOp LTHm, e1, e2, jmp)
        LE a -> emit(IfOp LEm, e1, e2,  jmp)
        GTH a -> emit(IfOp GTHm, e1, e2,  jmp)
        GE a -> emit(IfOp GEm, e1, e2, jmp)
        EQU a -> emit(IfOp EQUm, e1, e2, jmp)
        NE a -> emit(IfOp NEm, e1, e2, jmp)
    return ()

compl :: RelOp x -> RelOp x
compl (LTH a) = GE a
compl (LE a) = GTH a
compl (GTH a) =LE a
compl (GE a) = LTH a
compl (EQU a) =NE a
compl (NE a) = EQU a





genExpr :: Expr (Maybe(Int, Int))-> StateT EnvMid IO (Argument)
genExpr exp = case exp of
        EVar x lvalue@(ValVar a (Ident name)) -> do
            --temp <- freshTemp
            --emit(Load, Var (snd $ stringLValue lvalue) 0, temp, NIL)
            getVar name--temp
        EVar x lvalue@(ValArr a (Ident name) e) -> do
            index <- genExpr e
            t <- freshTemp
            emit(GetElemPtr, Var name 0 0, index, t)
            res <- freshTemp
            emit(Load, t, res, NIL)
            return res
        --ENew a type_ -> ENew (f a) (fmap f type_)
        --ENewArr a type_ expr -> ENewArr (f a) (fmap f type_) (fmap f expr)
        --ENullCast a type_ -> ENullCast (f a) (fmap f type_)
        EString a string -> return $ ValStr string
        ELitInt a integer -> return $ ValInt integer
        ELitTrue a -> return $ ValBool True
        ELitFalse a -> return $ ValBool False
        Neg a expr -> do
            e <- genExpr expr
            t <- freshTemp
            emit (NegOp, e, NIL, t)
            return t
        Not a expr -> do
            e <- genExpr expr
            t <- freshTemp
            emit (NotOp, e, NIL, t)
            return t
        EMul a expr1 mulop expr2 -> do
            e1 <- genExpr expr1
            e2 <- genExpr expr2
            t <- freshTemp
            case mulop of
                Times a -> emit(MulOp, e1, e2, t)
                Div a -> emit(DivOp, e1, e2, t)
                Mod a -> emit(ModOp, e1, e2, t)
            return t
        EAdd a expr1 addop expr2 -> do
            e1 <- genExpr expr1
            e2 <- genExpr expr2
            t <- freshTemp
            case addop of
                Plus a -> emit(AddOp, e1, e2, t)
                Minus a -> emit(SubOp, e1, e2, t)
            return t
        ERel a expr1 relop expr2 -> do
            e1 <- genExpr expr1
            e2 <- genExpr expr2
            lTrue <- reserveLabel "l"
            case (relop) of
                LTH x -> emit(IfOp LTHm, e1, e2, Label "l" lTrue)
                LE a -> emit(IfOp LEm, e1, e2,  Label "l" lTrue)
                GTH a -> emit(IfOp GTHm, e1, e2,  Label "l" lTrue)
                GE a -> emit(IfOp GEm, e1, e2, Label "l" lTrue)
                EQU a -> emit(IfOp EQUm, e1, e2,  Label "l" lTrue)
                NE a -> emit(IfOp NEm, e1, e2,  Label "l" lTrue)
            t <- freshTemp
            emit(AssOp, ValBool False, t, NIL)
            updateLabel lTrue
            emit(AssOp, ValBool True, t, NIL)
            return t

        EAnd a expr1 expr2 -> do
            e1 <- genExpr expr1
            lFalse <- reserveLabel "l"
            lEnd <- reserveLabel "l"
            emit(IfOp NEm, e1, ValBool True, Label "l" lFalse)
            e2 <- genExpr expr2
            emit(IfOp NEm, e2, ValBool True, Label "l" lFalse)
            t <- freshTemp
            emit(AssOp, ValBool True, t, NIL)
            emit(GotoOp, Label "l" lEnd, NIL, NIL)
            updateLabel lFalse
            emit(AssOp, ValBool False, t, NIL)
            updateLabel lEnd

            return t
        EOr a expr1 expr2 -> do
            e1 <- genExpr expr1
            lTrue <- reserveLabel "l"
            lEnd <- reserveLabel "l"
            emit(IfOp NEm, e1, ValBool True, Label "l" lTrue)
            e2 <- genExpr expr2
            emit(IfOp NEm, e2, ValBool True, Label "l"  lTrue)
            t <- freshTemp
            emit(AssOp, ValBool False, t, NIL)
            emit(GotoOp, Label "l" lEnd, NIL, NIL)
            updateLabel  lTrue
            emit(AssOp, ValBool True, t, NIL)
            updateLabel lEnd
            return t
        EApp a (Ident name) exprs -> do
              e <- mapM genExpr exprs
              mapM emitArg e
              t <- freshTemp
              emit(CallOp, Fun name, NIL, t)
              return t
        --EClApp a ident1 ident2 exprs -> EClApp (f a) ident1 ident2 (map (fmap f) exprs)
    --    EArrLen a ident -> EArrLen (f a) ident
    --    ERel a expr1 relop expr2 -> ERel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)



emitArg :: Argument -> StateT EnvMid IO ()
emitArg arg = do
    emit(ParamOp, arg, NIL, NIL)
    return ()
