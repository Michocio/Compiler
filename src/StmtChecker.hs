module StmtChecker where

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

import Environment
import FrontEndErrors
import ExprChecker
import TypesStuff

doBlock :: Ident -> Bool -> BlockD -> StateT Env IO Bool
doBlock _ ret (Block _ []) = do
    (a, b, c, d) <- get
    put(a, M.empty, c, d)
    return ret
doBlock funName ret (Block a (x:xs)) = do
    newRet <- runStmt funName x
    if(ret == False) then doBlock funName newRet (Block a xs)
        else doBlock funName ret (Block a xs)

runStmt :: Ident -> Stmt (Maybe(Int, Int)) -> StateT Env IO Bool
runStmt funName@(Ident name) (Ret info expr) = do
    a <- exprType expr
    (_, _, funs, _) <- get
    (desiredType, _, _) <- return $ fromJust $ M.lookup funName funs
    if((getType desiredType) == (getType a)) then return True
        else
            do
                liftIO $ putStrLn $ (show $ fromJust info) ++ ": Wrong return type in function " ++
                    name ++ ", expected " ++ (show (getType desiredType)) ++ " but given " ++
                    (show (getType a))
                liftIO exitFailure
runStmt funName@(Ident name) (VRet info) = do
    (_, _, funs, _) <- get
    (desiredType, _, _) <- return $ fromJust $ M.lookup funName funs
    if(getType desiredType == (Void Nothing)) then return True
        else
            do
                liftIO $ putStrLn $ (show $ fromJust info) ++ ": Wrong return type in not void function " ++
                    name
                liftIO exitFailure
runStmt funName@(Ident name) (Empty a) = return False
runStmt funName@(Ident name) (Cond _ cond ifBlock) = do
    condType <- exprType cond
    if((getType condType) == (Bool Nothing)) then do
        runStmt funName ifBlock
        return False
            else do
                liftIO $ putStrLn "Wrong expression in if condition"
                liftIO exitFailure
runStmt funName@(Ident name) (CondElse _ cond ifBlock elseBlock) = do
    condType <- exprType cond
    if((getType condType) == (Bool Nothing)) then do
        ifRet <- runStmt funName ifBlock
        elseRet <- runStmt funName elseBlock
        return ((True == ifRet) == elseRet)
            else do
                liftIO $ putStrLn "Wrong expression in if condition"
                liftIO exitFailure

runStmt funName@(Ident name) (While a cond block) = do
    runStmt funName (Cond a cond block)
runStmt _ (SExp a expr) = do
    exprType expr
    return False
runStmt _ (Decl a vType items) = do
    x <- mapM (itemOperator vType) items
    return False
runStmt _ (Ass a lvalue expr) = do
    lType <- exprType (EVar a lvalue)
    rType <- exprType expr
    liftIO $ putStrLn $ show lType
    liftIO $ putStrLn $ show rType
    if((getType lType) /=  getType rType) then do
        liftIO $ putStrLn $ (show $ fromJust a) ++ ": Wrong type of value to assign"
        liftIO exitFailure
    else return False
runStmt fun (BStmt a block) = doBlock fun False block
runStmt _ (Incr a lvalue) = do
    varType <- exprType (EVar a lvalue)
    if(getType varType /= (Int Nothing)) then do
        liftIO $ putStrLn $ (show $ fromJust a) ++ ": Int variable needed in that operation"
        liftIO exitFailure
    else return False
runStmt x (Decr a lvalue) = runStmt x (Incr a lvalue)
runStmt _ _ = return False


itemOperator :: TypeD -> Item (Maybe (Int, Int))-> StateT Env IO ()
itemOperator t (NoInit info name) = do
    (vars, decls, funs, classes)<- get
    case (M.lookup name decls) of
        (Just x) -> do
            liftIO $ putStrLn "TUUUUUUUUUUUUUUUUUUERTERGDF"
            liftIO $ putStrLn $ (show $ fromJust info) ++ ": Redeclaration of previously declared variable: "
                ++ (show name)
            liftIO exitFailure
        (otherwise) -> do
            put (M.insert name t vars, M.insert name t vars, funs, classes)
            return ()

itemOperator t (Init info name expr) = do
    expType <- exprType expr
    if((getType t) /= (getType $ expType)) then do
        liftIO $ putStrLn $ (show $ fromJust info) ++ ": Wrong type of variable to assign"
        liftIO exitFailure
    else itemOperator t (NoInit info name)
