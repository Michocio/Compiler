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

generateIntermediate :: ProgramD -> IO ()
generateIntermediate p =  (liftIO $ evalStateT (genProgram p) initialEnvMid)

genProgram :: ProgramD -> StateT EnvMid IO ()
genProgram (Program info []) = return ()
genProgram  (Program info (x:xs)) = genTopDef (x:xs)

genTopDef :: [TopDefD] -> StateT EnvMid  IO ()
genTopDef [] = do
    (a, b, c, d, e) <- get
    lines_ <- return $ map (printTuple) d
    lin <-return $  lines_
    labels <- return $  M.toList e
    labels_ <- return $ map swap labels
    so <- return $ sortBy (\((_, pos1), _) ((_, pos2), _) -> compare pos1 pos2) labels_
    ok <- return $ insertLabels 0 so lin
    liftIO $ mapM print ok
    liftIO $ putStrLn $ show e
genTopDef  (x:xs) = do
    genTopInstr x
    genTopDef xs

genTopInstr :: TopDefD -> StateT EnvMid IO ()
genTopInstr fun@(FnDef a type_ ident@(Ident name) args (Block t block)) = do
    emit(Function, Fun name, NIL, NIL)
    mapM genStmt block
    return ()

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
    lEnd_ <- reserveLabel "l"
    lEnd <- return $ Label  "l" lEnd_
    lTrue_ <- reserveLabel "l"
    lTrue <- return $ Label  "l" lTrue_
    genCond cond lTrue lEnd lEnd
    updateLabel lTrue_
    genStmt (BStmt Nothing $ Block Nothing [ifBlock])
    updateLabel lEnd_
genStmt (CondElse info cond ifBlock elseBlock) = do
    lFalse_ <- reserveLabel "l"
    lFalse <- return $ Label  "l" lFalse_
    lTrue_ <- reserveLabel "l"
    lTrue <- return $ Label  "l" lTrue_
    lEnd_ <- reserveLabel "l"
    lEnd <- return $ Label  "l" lEnd_
    genCond cond lTrue lFalse lEnd
    updateLabel lTrue_
    genStmt (BStmt Nothing $ Block Nothing [ifBlock])
    updateLabel lFalse_
    genStmt (BStmt Nothing $ Block Nothing [elseBlock])
    updateLabel lEnd_

genCond :: Expr (Maybe (Int, Int)) -> Argument -> Argument -> Argument -> StateT EnvMid IO (Int)
genCond (EAnd _ e1 e2) lTrue lFalse lNext = do
    lMid_ <- (reserveLabel "l")
    lMid <- return $ Label  "l" lMid_
    n <- genCond e1 lMid lFalse lMid
    updateLabel lMid_
    genCond e2 lTrue lFalse (Label "l" n)
    return lMid_
genCond (EOr _ e1 e2) lTrue lFalse lNext = do
    lMid_ <- (reserveLabel "l")
    lMid <- return $ Label  "l" lMid_
    n <- genCond e1 lTrue lMid lMid
    updateLabel lMid_
    genCond e2 lTrue lFalse (Label "l" n)
    return lMid_

genCond (Not _ e) lTrue lFalse lNext = genCond e lFalse lTrue lNext

genCond (ERel a expr1 relop expr2 ) lThen lElse lNext= do
    e1 <- genExpr expr1
    e2 <- genExpr expr2
    case (relop) of
        LTH x -> emit(IfOp LTHm, e1, e2, lThen)
        LE a -> emit(IfOp LEm, e1, e2,  lThen)
        GTH a -> emit(IfOp GTHm, e1, e2,  lThen)
        GE a -> emit(IfOp GEm, e1, e2, lThen)
        EQU a -> emit(IfOp EQUm, e1, e2,  lThen)
        NE a -> emit(IfOp NEm, e1, e2,  lThen)
    emit(GotoOp, lElse, NIL, NIL)
    return 1



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
