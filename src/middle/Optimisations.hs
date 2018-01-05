module Optimisations where

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

--type SimpleBlock = (Argument, [Tuple])
--type ControlFlow = (Argument, [Argument])

replaceBy :: [Tuple] ->  (Argument, Argument) -> [Tuple]
replaceBy  code  (src, dst) =
    map (changeRegs (src, dst)) code

changeRegs :: (Argument, Argument) -> Tuple -> Tuple
changeRegs (src, dst) (AssOp, arg1, res, arg3) =
    (AssOp, isDesiredArg arg1 src dst, res, arg3)
changeRegs (src, dst) (Alloca t, arg1, res, arg3) =
    (Alloca t, arg1, res, arg3)
changeRegs (src, dst) (op, arg1, arg2, arg3) =
    constExpr (op, isDesiredArg arg1 src dst, isDesiredArg arg2 src dst, isDesiredArg arg3 src dst)

isDesiredArg :: Argument -> Argument -> Argument -> Argument
isDesiredArg x y z = if(x==y) then z else x

addLife :: Argument -> [Argument]
addLife NIL = []
addLife a@(Var x y z) = [a]
addLife a@(Reg x) = [a]
addLife x = []

removeDeadUsage :: [Argument] -> Tuple -> Maybe Tuple
removeDeadUsage ok (AssOp, arg1, res, arg3) =
    if ((elem res ok) == False) then Nothing
    else (Just (AssOp, arg1, res, arg3))
removeDeadUsage ok (Alloca z, arg1, res, arg3) =
    if ((elem arg1 ok) == False) then Nothing
    else (Just (Alloca z, arg1, res, arg3))
removeDeadUsage a b = Just b
aliveVar :: ([Argument], [Argument]) -> [Tuple] -> ([Argument], [Argument])
aliveVar  (declared, used) []= (nub declared, nub used)
aliveVar (declared, used) ((AssOp, src, dst, NIL):xs) = aliveVar (declared++[dst], used) xs
aliveVar (declared, used) ((Alloca _, dst, NIL, NIL):xs)= aliveVar (declared++[dst], used) xs
aliveVar (declared, used) ((op, a1, a2, a3):xs)=
    aliveVar (declared, used ++ (addLife a1)++ (addLife a2)++ (addLife a3)) xs

doOpt :: [[Tuple]] ->  StateT EnvMid IO [[Tuple]]
doOpt code = do
    new_code <- constOpt code
    liftIO $ putStrLn "oooo"
    liftIO $ mapM (mapM (print. printTuple)) code
    liftIO $ putStrLn "new"
    liftIO $ mapM (mapM (print. printTuple)) new_code
    if(new_code /= code) then doOpt new_code
    else return new_code
constOpt :: [[Tuple]] ->  StateT EnvMid IO [[Tuple]]
constOpt code = do
    consts <- return $ foldr (++) [] (map (map constAss) code)
    pary <- return $ map (fromJust) (filter (isJust) consts)
    liftIO $ putStrLn $ show pary
    blocks <- return $ map (correctCode pary) code
    return blocks

--constFolding :: [Tuple] -> StateT EnvMid IO [Tuple]
--constFolding code = do
--    return $ correctCode code consts

correctCode :: [(Argument, Argument)] -> [Tuple] ->  [Tuple]
correctCode [] code= code
correctCode (x:xs) code =
    correctCode xs (replaceBy code x)

constExpr :: Tuple -> Tuple
constExpr (AndOp, ValBool i1, ValBool i2, res) =
    (AssOp, ValBool (i1 && i2), res, NIL)
constExpr (OrOp, ValBool i1, ValBool i2, res) =
    (OrOp, ValBool (i1 || i2), res, NIL)
constExpr (AddOp, ValInt i1, ValInt i2, res) =
    (AssOp, ValInt (i1 + i2), res, NIL)
constExpr (SubOp, ValInt i1, ValInt i2, res) =
    (AssOp, ValInt (i1 - i2), res, NIL)
constExpr (DivOp, ValInt i1, ValInt i2, res) =
    (AssOp, ValInt (div i1 i2), res, NIL)
constExpr (MulOp, ValInt i1, ValInt i2, res) =
    (AssOp, ValInt (i1 * i2), res, NIL)
constExpr (ModOp, ValInt i1, ValInt i2, res) =
    (AssOp, ValInt (mod i1 i2), res, NIL)
constExpr (NegOp, ValInt i1, res, _) =
    (AssOp, ValInt ((-1) *i1), res, NIL)
constExpr (NotOp, ValBool i1, res, _) =
    (AssOp, ValBool (not i1), res, NIL)
constExpr x = x


constAss :: Tuple -> Maybe (Argument, Argument)
constAss (AssOp, src, dst, _) =
    if(isConst src) then Just (dst, src)
    else Nothing
constAss _ = Nothing

isConst :: Argument -> Bool
isConst (ValInt _) = True
isConst (ValBool _) = True
isConst (ValStr _) = True
isConst (ValVoid) = True
isConst _ = False
