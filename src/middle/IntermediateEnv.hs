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
  deriving (Eq, Ord, Show, Read)
data CmpOp  = LTHm | LEm | GTHm | GEm | EQUm | NEm
     deriving (Eq, Ord, Show, Read)

data Argument =
    NIL | Reg Int | Var String |
    ValInt Integer | ValBool Bool | ValStr String | ValVoid |
    Label String Int | Fun String
  deriving (Eq, Ord, Show, Read)

type Tuple = (Operand, Argument, Argument, Argument)

type Vars = M.Map String Int
type Labels = M.Map Int (String, Int)
type EnvMid = (Vars, Int, Int, [Tuple], Labels)
initialEnvMid = (M.empty, 0::Int, 0::Int, [], M.empty)

emit :: Tuple -> StateT EnvMid IO ()
emit tuple = do
    (a, b, c, code, e) <- get
    put(a, b, c, code ++ [tuple], e)

incTemps :: StateT EnvMid IO ()
incTemps = do
    (a, b, c, d, e) <- get
    put(a, b + 1, c, d, e)

getTemp :: StateT EnvMid IO (Int)
getTemp = do
    (_, b, _, _, _) <- get
    return b

numberOfLine ::  StateT EnvMid IO (Int)
numberOfLine = do
    (_, _, _, code, _) <- get
    return $ (length code) - 1

freshTemp :: StateT EnvMid IO (Argument)
freshTemp = do
    num <- getTemp
    incTemps
    return $ Reg num

genLabel :: String -> StateT EnvMid IO (Int)
genLabel name = do
    (a, b, label_num, code, labels) <- get
    numberOfLine <- return $ length code
    put(a, b, label_num + 1, code, M.insert label_num (name, numberOfLine) labels)
    return label_num

reserveLabel :: String -> StateT EnvMid IO (Int)
reserveLabel label = do
    (a, b, label_num, code, labels) <- get
    numberOfLine <- return $ length code
    put(a, b, label_num + 1, code, M.insert label_num (label, (-1))  labels)
    return label_num

updateLabel :: Int -> StateT EnvMid IO ()
updateLabel nr = do
    (a, b, c, code, labels) <- get
    (Just (name, _)) <- return $ M.lookup nr labels
    line <- numberOfLine
    put(a, b, c, code, M.insert nr (name, (line + 1)) labels)
    return ()

insertLabels :: Int -> [((String, Int), Int)] -> [String] -> [String]
insertLabels _ [] a = a
insertLabels i (((lab, pos), ins):xs) curr = insertLabels (i+1) xs (insertElement (pos + i) (lab ++ (show ins) ++ " : ") curr )



printArg :: Argument -> String
printArg NIL = "nil"
printArg (Reg i) = "t" ++ (show i)
printArg (Var s) = s
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
printTuple (op@CallOp, a1, _, _) =  "     " ++(show op)++ " " ++(printArg a1)
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
