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


import Environment
import TypesStuff
import IntermediateEnv
import Misc

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
    --line <- numberOfLine
    --l <- genLabel ("Funkcja " ++ name)
    --emit(Function, Fun name, NIL, NIL)
    put initialEnvMid
    mapM genStmt block
    --line_ <- numberOfLine
    --if(line == line_) then emit(EmptyOp, NIL, NIL, NIL)
    --else return ()
    liftIO $ putStrLn $ "##########" ++ (show name) ++ ": "
    printCode
    return ()

printCode :: StateT EnvMid IO ()
printCode = do
    (a, b, c, d, e) <- get
    if(M.size e == 0) then put (a, b, c+1, d, M.insert 0 ("entry", 0) e) else return ()
    z <- genLabel "end"
    (a, b, c, d, e) <- get
    lines_ <- return $ map (printTuple) d
    lin <-return $  lines_
    labels <- return $  M.toList e
    labels_ <- return $ map swap labels
    so <- return $ sortBy (\((_, pos1), _) ((_, pos2), _) -> compare pos1 pos2) labels_
    ok <- return $ insertLabels 0 so lin
    liftIO $ mapM print ok
    liftIO $ putStrLn $ show e
    starts <- return $ sort $ map (\(_, pos) -> pos) (M.elems e)
    ends <- return $ sort $ returnJump 0 [] d
    --liftIO $ putStrLn $ show starts
    ext <- return $  tail $ (sort $ ends ++ (map (subtract 1) starts))
    len <- numberOfLine
    begs <- return $ filter (\(a) -> (a <= len)) starts
    endings <- return $ filter (\(a) -> (a >= 0)) ext
    begs_ <- return $(take (length endings) begs)
    ends_ <- return $ (take (length begs_) endings)
    liftIO  $ putStrLn $ show begs_
    liftIO  $ putStrLn $ show ends_


genStmt ::Stmt (Maybe(Int, Int)) -> StateT EnvMid IO ()
genStmt  (BStmt x (Block a stmts)) = do
    mapM genStmt stmts
    return ()
genStmt  (SExp a expr) = do
    genExpr expr
    return ()
genStmt  (Ret info expr) = do
    ret <- genExpr expr
    emit(RetOp, ret, NIL, NIL)
genStmt  (VRet info) = emit(RetOp, NIL, NIL, NIL)
genStmt (Decl _ type_ items_) = do
    x <- mapM genItem items_
    return ()
genStmt (Ass x lvalue@(ValArr a (Ident name) e) expr) = do
    val <- genExpr expr
    index <- genExpr e
    t <- freshTemp
    emit(GetElemPtr, Var name, index, t)
    emit(Store, val, t, NIL)
genStmt (Ass x var@(ValVar a name) expr) = do
    res <- genExpr expr
    emit(Store, res, Var (snd $ stringLValue var), NIL)
genStmt (Incr a val) = do
    temp <- freshTemp
    emit(Load, Var (snd $ stringLValue val), temp, NIL)
    t <- freshTemp
    emit(AddOp, temp, ValInt 1, t)
    emit(Store, t, Var (snd $ stringLValue val), NIL)
genStmt (Decr a val) = do
    temp <- freshTemp
    emit(Load, Var (snd $ stringLValue val), temp, NIL)
    t <- freshTemp
    emit(SubOp, temp, ValInt 1, t)
    emit(Store, t, Var (snd $ stringLValue val), NIL)
genStmt  (Empty a) = return ()
genStmt  (Cond info cond ifBlock) = do
    case (cond) of
        (ELitTrue a) -> do
            lTrue_ <- genLabel "l"
            genStmt (BStmt Nothing $ Block Nothing [ifBlock])
        (ELitFalse a) -> return ()
        otherwise -> do
            lTrue_ <- reserveLabel "l"
            lTrue <- return $ Label  "l" lTrue_
            lEnd_ <- reserveLabel "l"
            lEnd <- return $ Label  "l" lEnd_
            genCond cond lTrue lEnd lEnd
            updateLabel lTrue_
            genStmt (BStmt Nothing $ Block Nothing [ifBlock])
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
            genCond cond lTrue lFalse lFalse
            updateLabel lTrue_
            genStmt (BStmt Nothing $ Block Nothing [ifBlock])
            updateLabel lFalse_
            genStmt (BStmt Nothing $ Block Nothing [elseBlock])
            updateLabel lEnd_
genStmt (While a expr stmt) = do
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



genItem ::Item (Maybe (Int, Int)) -> StateT EnvMid IO ()
genItem  (Init info (Ident name) expr) = do
    res <- genExpr expr
    emit(Store, res, Var name, NIL)
genItem _ =  return ()



genExpr :: Expr (Maybe(Int, Int))-> StateT EnvMid IO (Argument)
genExpr exp = case exp of
        EVar x lvalue@(ValVar a name) -> do
            temp <- freshTemp
            emit(Load, Var (snd $ stringLValue lvalue), temp, NIL)
            return temp
        EVar x lvalue@(ValArr a (Ident name) e) -> do
            index <- genExpr e
            t <- freshTemp
            emit(GetElemPtr, Var name, index, t)
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
            lTrue <- reserveLabel "lTrue"
            case (relop) of
                LTH x -> emit(IfOp LTHm, e1, e2, Label "lTrue" lTrue)
                LE a -> emit(IfOp LEm, e1, e2,  Label "lTrue" lTrue)
                GTH a -> emit(IfOp GTHm, e1, e2,  Label "lTrue" lTrue)
                GE a -> emit(IfOp GEm, e1, e2, Label "lTrue" lTrue)
                EQU a -> emit(IfOp EQUm, e1, e2,  Label "lTrue" lTrue)
                NE a -> emit(IfOp NEm, e1, e2,  Label "lTrue" lTrue)
            t <- freshTemp
            emit(AssOp, ValBool False, t, NIL)
            updateLabel lTrue
            emit(AssOp, ValBool True, t, NIL)
            return t

        EAnd a expr1 expr2 -> do
            e1 <- genExpr expr1
            lFalse <- reserveLabel "lFalse"
            lEnd <- reserveLabel "lEnd"
            emit(IfOp NEm, e1, ValBool True, Label "lFalse" lFalse)
            e2 <- genExpr expr2
            emit(IfOp NEm, e2, ValBool True, Label "lFalse" lFalse)
            t <- freshTemp
            emit(AssOp, ValBool True, t, NIL)
            emit(GotoOp, Label "lEnd" lEnd, NIL, NIL)
            updateLabel lFalse
            emit(AssOp, ValBool False, t, NIL)
            updateLabel lEnd
            return t
        EOr a expr1 expr2 -> do
            e1 <- genExpr expr1
            lTrue <- reserveLabel "lTrue"
            lEnd <- reserveLabel "lEnd"
            emit(IfOp NEm, e1, ValBool True, Label "lTrue" lTrue)
            e2 <- genExpr expr2
            emit(IfOp NEm, e2, ValBool True, Label "lTrue" lTrue)
            t <- freshTemp
            emit(AssOp, ValBool False, t, NIL)
            emit(GotoOp, Label "lEnd" lEnd, NIL, NIL)
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
