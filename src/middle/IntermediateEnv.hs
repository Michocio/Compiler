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
  deriving (Eq, Ord, Show, Read)
data CmpOp  = LTHm | LEm | GTHm | GEm | EQUm | NEm
     deriving (Eq, Ord, Show, Read)

data Argument =
    NIL | Reg Int | Var String Int |
    ValInt Integer | ValBool Bool | ValStr String | ValVoid |
    Label String Int | Fun String
  deriving (Eq, Ord, Show, Read)


type Tuple = (Operand, Argument, Argument, Argument)

type Vars = M.Map String Int
type Labels = M.Map Int (String, Int)
type EnvMid = (Vars, Vars, Int, Int, [Tuple], Labels)

initialEnvMid = (M.empty, M.empty, 0::Int, 0::Int, [], M.empty)

deleteLabel :: Int -> StateT EnvMid IO ()
deleteLabel nr = do
    (vars, decls, temps, lab, code, labels) <- get
    put(vars, decls, temps, lab, code, M.delete nr labels)
    return ()

getVar ::  String -> StateT EnvMid IO (Argument)
getVar name = do
    (vars, decls, a, b, c, d) <- get
    case (M.lookup name decls) of
        Nothing -> do
            return (Var name (fromJust $ M.lookup name vars))
        (Just x) -> return (Var name x)

genVar :: String -> StateT EnvMid IO (Argument)
genVar name = do
    (vars, decls, a, b, c, d) <- get
    case(M.lookup name vars) of
        Nothing -> do
            put (M.insert name 0 vars , decls, a, b, c, d)
            return (Var name 0)
        (Just x) -> do
            put (vars, M.insert name (x + 1) decls, a, b, c, d)
            return (Var name (x+1))


getLabels :: StateT EnvMid IO (Labels)
getLabels = do
    (_, _, _, _, _, labels) <- get
    return labels

getCode :: StateT EnvMid IO ([Tuple])
getCode = do
    (_, _, _, _, code, _) <- get
    return code

emit :: Tuple -> StateT EnvMid IO ()
emit tuple = do
    (vars, decls, temps, lab, code, labels) <- get
    put(vars, decls, temps, lab, code ++ [tuple], labels)

incTemps :: StateT EnvMid IO ()
incTemps = do
    (vars, decls, temps, lab, code, labels) <- get
    put(vars, decls, temps + 1, lab, code, labels)

getTemp :: StateT EnvMid IO (Int)
getTemp = do
    (vars, decls, temps, lab, code, labels) <- get
    return temps

numberOfLine ::  StateT EnvMid IO (Int)
numberOfLine = do
    (vars, decls, temps, lab, code, labels) <- get
    return $ (length code) - 1

freshTemp :: StateT EnvMid IO (Argument)
freshTemp = do
    num <- getTemp
    incTemps
    return $ Reg num

genLabel :: String -> StateT EnvMid IO (Int)
genLabel name = do
    (vars, decls, temps, label_num, code, labels) <- get
    numberOfLine <- return $ length code
    put(vars, decls, temps, label_num + 1, code, M.insert label_num (name, numberOfLine) labels)
    return label_num

putEntryLabel :: StateT EnvMid IO ()
putEntryLabel = do
    (vars, decls, temps, label_num, code, labels) <- get
    if(M.size labels == 0) then do
        put (vars, decls, temps, label_num + 1, code, M.insert 0 ("entry", 0) labels)
        return ()
    else do
        case (M.lookup 0 labels) of
            (Just (_, 0)) -> return ()
            otherwise ->
                put (vars, decls, temps, label_num + 1, code, M.insert label_num ("entry", 0) labels)
        return ()

showCode :: StateT EnvMid IO ([((String, Int), Int)])
showCode = do
    (vars, decls, temps, label_num, code, labels) <- get
    lines_ <- return $ map (printTuple) code
    labels_pos_ <- return $  M.toList labels
    labels_pos <- return $ map swap labels_pos_
    sortedLabels <- return $ sortBy (\((_, pos1), _) ((_, pos2), _) -> compare pos1 pos2) labels_pos
    withLabels <- return $ insertLabels 0 sortedLabels lines_
    liftIO $ mapM print withLabels
    return sortedLabels

reserveLabel :: String -> StateT EnvMid IO (Int)
reserveLabel label = do
    (vars, decls, temps, label_num, code, labels) <- get
    numberOfLine <- return $ length code
    put(vars, decls, temps, label_num + 1, code, M.insert label_num (label, (-1))  labels)
    return label_num

updateLabel :: Int -> StateT EnvMid IO ()
updateLabel nr = do
    (vars, decls, temps, label_num, code, labels) <- get
    (Just (name, _)) <- return $ M.lookup nr labels
    line <- numberOfLine
    put(vars, decls, temps, label_num, code, M.insert nr (name, (line + 1)) labels)
    return ()

insertLabels :: Int -> [((String, Int), Int)] -> [String] -> [String]
insertLabels _ [] a = a
insertLabels i (((lab, pos), ins):xs) curr = insertLabels (i+1) xs (insertElement (pos + i) (lab ++ (show ins) ++ " : ") curr )



printArg :: Argument -> String
printArg NIL = "nil"
printArg (Reg i) = "t" ++ (show i)
printArg (Var s n) = s ++"_" ++(show n)
printArg (ValInt i) = show i
printArg (ValBool b) = show b
printArg (ValStr s) = s
printArg (ValVoid) = "void"
printArg (Label s num) = s ++ (show num)
printArg (Fun s) = s

printTuple :: Tuple -> String
printTuple (op@AndOp, a1, a2, res) = "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@AssOp, a1, res, _) = "     " ++
    (printArg res) ++ " = " ++(printArg a1)
printTuple (op@OrOp, a1, a2, res) =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@AddOp, a1, a2, res) =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@SubOp, a1, a2, res) =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@DivOp, a1, a2, res) =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@MulOp, a1, a2, res) =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@ModOp, a1, a2, res) =  "     " ++
    (printArg res) ++ " = " ++(printArg a1) ++ " "++ (show op) ++" " ++ (printArg a2)
printTuple (op@NegOp, a1, a2, res) =  "     " ++
    (printArg res) ++ " = " ++ (show op)++ " " ++(printArg a1)
printTuple (op@NotOp, a1, a2, res) =  "     " ++
    (printArg res) ++ " = " ++ (show op)++ " " ++(printArg a1)
printTuple (op@GotoOp, a1, _, _) =  "     " ++(show op)++ " " ++(printArg a1)
printTuple (op@ParamOp, a1, _, _) =  "     " ++(show op)++ " " ++(printArg a1)
printTuple (op@CallOp, a1, _, _) =  "     " ++(show op)++ " " ++(printArg a1)
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
printTuple (Alloca t, dst, _, _) = "     " ++(printArg dst) ++ " = " ++ "alloca " ++ (show t)
