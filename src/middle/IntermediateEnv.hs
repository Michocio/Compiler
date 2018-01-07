module IntermediateEnv where

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
import Misc

data Operand
    = AndOp
    | OrOp
    | AddOp
    | SubOp
    | DivOp
    | MulOp
    | ModOp
    | NegOp
    | NotOp
    | AssOp
    | GotoOp
    | IfOp CmpOp
    | ParamOp
    | CallOp
    | GetElemPtr
    | RetOp
    | Load
    | Store
    | Function
    | EmptyOp
    | Alloca (Type (Maybe (Int, Int)))
    | Phi
  deriving (Eq, Ord, Show, Read)
data CmpOp  = LTHm | LEm | GTHm | GEm | EQUm | NEm
     deriving (Eq, Ord, Show, Read)

data Argument =
    NIL | Reg Int | Var String Int Int Int |
    ValInt Integer | ValBool Bool | ValStr String | ValVoid |
    Label String Int | Fun String | From Int Argument | SSA [Argument]
  deriving (Eq, Ord, Show, Read)


type Tuple = (Operand, Argument, Argument, Argument)

type Vars = M.Map String Int
type Labels = M.Map Int (String, Int)
type EnvMid = (Vars, Vars, Int, Int, [Tuple], Labels, Vars)

initialEnvMid = (M.empty, M.empty, 0::Int, 0::Int, [], M.empty, M.empty)

deleteLabel :: Int -> StateT EnvMid IO ()
deleteLabel nr = do
    (vars, decls, temps, lab, code, labels, curr) <- get
    put(vars, decls, temps, lab, code, M.delete nr labels, curr)
    return ()

assingVar ::  String -> StateT EnvMid IO (Argument)
assingVar name = do
    (vars, decls, a, b, c, d, curr) <- get
    num <- return (fromJust $ M.lookup name curr)
    x <- return (fromJust $M.lookup name decls)
    put(vars, M.insert name (x+1) decls, a, b, c, d, curr)
    return (Var name num (x+1) 0)


getVar ::  String -> StateT EnvMid IO (Argument)
getVar name = do
    (vars, decls, a, b, c, d, curr) <- get
    num <- return (fromJust $ M.lookup name curr)
    occurs <- return (fromJust $ M.lookup name decls)
    return (Var name num occurs 0)


newVar :: String -> StateT EnvMid IO (Argument)
newVar name = do
    (vars, decls, a, b, c, d, current) <- get
    num <- return (M.lookup name vars)
    case (num) of
        Nothing -> do
            put (M.insert name 0 vars, M.insert name 0 decls, a, b, c, d, M.insert name 0 current)
            return (Var name 0 0 0)
        (Just x) -> do
            put ( M.insert name (x+1) vars, M.insert name 0 decls, a, b, c, d, M.insert name (x+1) current)
            return (Var name (x+1) 0 0)


getLabels :: StateT EnvMid IO (Labels)
getLabels = do
    (_, _, _, _, _, labels, _) <- get
    return labels

getCode :: StateT EnvMid IO ([Tuple])
getCode = do
    (_, _, _, _, code, _, _) <- get
    return code

emit :: Tuple -> StateT EnvMid IO ()
emit tuple = do
    (vars, decls, temps, lab, code, labels, e) <- get
    put(vars, decls, temps, lab, code ++ [tuple], labels, e)

incTemps :: StateT EnvMid IO ()
incTemps = do
    (vars, decls, temps, lab, code, labels, e) <- get
    put(vars, decls, temps + 1, lab, code, labels, e)

getTemp :: StateT EnvMid IO (Int)
getTemp = do
    (vars, decls, temps, lab, code, labels, e) <- get
    return temps

numberOfLine ::  StateT EnvMid IO (Int)
numberOfLine = do
    (vars, decls, temps, lab, code, labels, e) <- get
    return $ (length code) - 1

freshTemp :: StateT EnvMid IO (Argument)
freshTemp = do
    num <- getTemp
    incTemps
    return $ Reg num

genLabel :: String -> StateT EnvMid IO (Int)
genLabel name = do
    (vars, decls, temps, label_num, code, labels, e) <- get
    numberOfLine <- return $ length code
    put(vars, decls, temps, label_num + 1, code, M.insert label_num (name, numberOfLine) labels, e)
    return label_num

putEntryLabel :: StateT EnvMid IO ()
putEntryLabel = do
    (vars, decls, temps, label_num, code, labels, e) <- get
    if(M.size labels == 0) then do
        put (vars, decls, temps, label_num + 1, code, M.insert 0 ("entry", 0) labels, e)
        return ()
    else do
        case (M.lookup 0 labels) of
            (Just (_, 0)) -> return ()
            otherwise ->
                put (vars, decls, temps, label_num + 1, code, M.insert label_num ("entry", 0) labels, e)
        return ()

showCode :: StateT EnvMid IO ([((String, Int), Int)])
showCode = do
    (vars, decls, temps, label_num, code, labels, e) <- get
    lines_ <- return $ map (printTuple) code
    labels_pos_ <- return $  M.toList labels
    labels_pos <- return $ map swap labels_pos_
    sortedLabels <- return $ sortBy (\((_, pos1), _) ((_, pos2), _) -> compare pos1 pos2) labels_pos
    withLabels <- return $ insertLabels 0 sortedLabels lines_
    liftIO $ mapM print withLabels
    return sortedLabels

reserveLabel :: String -> StateT EnvMid IO (Int)
reserveLabel label = do
    (vars, decls, temps, label_num, code, labels, e) <- get
    numberOfLine <- return $ length code
    put(vars, decls, temps, label_num + 1, code, M.insert label_num (label, (-1))  labels, e)
    return label_num

updateLabel :: Int -> StateT EnvMid IO ()
updateLabel nr = do
    (vars, decls, temps, label_num, code, labels, e) <- get
    (Just (name, _)) <- return $ M.lookup nr labels
    line <- numberOfLine
    put(vars, decls, temps, label_num, code, M.insert nr (name, (line + 1)) labels, e)
    return ()

insertLabels :: Int -> [((String, Int), Int)] -> [String] -> [String]
insertLabels _ [] a = a
insertLabels i (((lab, pos), ins):xs) curr = insertLabels (i+1) xs (insertElement (pos + i) (lab ++ (show ins) ++ " : ") curr )



printArg :: Argument -> String
printArg NIL = "nil"
printArg (Reg i) = "t" ++ (show i)
printArg (Var s n i d) = s ++"_" ++(show n) ++ "_" ++ (show i)  ++ "_" ++ (show d)
printArg (ValInt i) = show i
printArg (ValBool b) = show b
printArg (ValStr s) = s
printArg (ValVoid) = "void"
printArg (Label s num) = s ++ (show num)
printArg (Fun s) = s
printArg (From x b) = "[" ++ show x ++", " ++ (show b) ++"]"
printArg (SSA x) = foldr (++) "" $ map printArg x

printTuple :: Tuple -> String
printTuple (op@AndOp, res, a1, a2) = "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@AssOp, res, a1, _) = "     " ++
    (printArg res) ++ " = " ++(printArg a1)
printTuple (op@OrOp, res, a1, a2)  =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@AddOp, res, a1, a2)  =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@SubOp, res, a1, a2)  =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@DivOp, res, a1, a2)  =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@MulOp, res, a1, a2)  =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@ModOp, res, a1, a2)  =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@NegOp, res, a1, a2)  =  "     " ++
    (printArg res) ++ " = " ++ (show op)++ " " ++(printArg a1)
printTuple (op@NotOp, res, a1, a2)  =  "     " ++
    (printArg res) ++ " = " ++ (show op)++ " " ++(printArg a1)
printTuple (op@GotoOp, a1, _, _) =  "     " ++(show op)++ " " ++(printArg a1)
printTuple (op@ParamOp, a1, _, _) =  "     " ++(show op)++ " " ++(printArg a1)
printTuple (op@CallOp, a1, x, _) =  "     " ++ (show x)++" = "++(show op)++ " " ++(printArg a1)
printTuple (op@RetOp, a1, _, _) =  "     " ++(show op)++ " " ++(printArg a1)
printTuple (op@(IfOp how), a1, a2, jmp) = "     " ++
    ("If") ++ " " ++ " " ++ (printArg a1) ++ " " ++ (show how)++" " ++ (printArg a2)
        ++ " jump " ++ (printArg jmp)
printTuple (op@(Load), a1, res, _) = "     " ++
    (show op) ++ " " ++ (printArg a1)  ++ " " ++ (printArg res)
printTuple (op@(Store), a1, res, _) = "     " ++
    (show op) ++ " " ++ (printArg a1)  ++ " " ++ (printArg res)
printTuple (op@(Function), a1, _, _) = ("########Funkcja   :" ++ (printArg a1))
printTuple (op@(GetElemPtr), arr, index, res) =  "     " ++
    (printArg res) ++ " = " ++ ("GetElemPtr") ++ " " ++ " " ++ (printArg arr) ++ " " ++ " " ++ (printArg index)
printTuple (EmptyOp, _, _, _) = "Empty"
printTuple (Alloca t, dst, _, _) = "     " ++ (printArg dst) ++ " = " ++ "alloca " -- ++ (show t)
printTuple (Phi, var, a, b) =  "     " ++  (printArg var) ++ " = Phi" ++ " " ++ (printArg a) ++ " "
