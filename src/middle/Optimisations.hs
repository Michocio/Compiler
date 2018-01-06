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
changeRegs (src, dst) (AssOp, res, arg1, NIL) =
    (AssOp,res, isDesiredArg src dst arg1 , NIL)
changeRegs (src, dst) (Alloca t, arg1, res, arg3) =
    (Alloca t, arg1, res, arg3)
changeRegs (src, dst) (Phi, res, SSA xs, a) =
    (Phi, res, SSA $ map (\(From b x) -> From b (isDesiredArg src dst x)) xs, a)
changeRegs (src, dst) (op, arg1, arg2, arg3) =
    constExpr (op, isDesiredArg src dst arg1, isDesiredArg src dst arg2, isDesiredArg src dst arg3)

isDesiredArg :: Argument -> Argument -> Argument -> Argument
isDesiredArg y z x = if(x==y) then z else x

addLife :: Argument -> [Argument]
addLife NIL = []
addLife a@(Var x y z i) = [a]
addLife a@(Reg x) = [a]
addLife x = []

removeDeadUsage :: [Argument] -> Tuple -> Maybe Tuple
removeDeadUsage ok (AssOp, res, arg1, arg3) =
    if ((elem res ok) == False) then Nothing
    else (Just (AssOp, res, arg1, arg3))
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
    new_code_ <- copyOpt new_code
--    liftIO $ putStrLn "oooo"
--    liftIO $ mapM (mapM (print. printTuple)) code
--    liftIO $ putStrLn "new"
--    liftIO $ mapM (mapM (print. printTuple)) new_code
    if(new_code_ /= code) then doOpt new_code_
    else return new_code_

constOpt :: [[Tuple]] ->  StateT EnvMid IO [[Tuple]]
constOpt code = do
    consts <- return $ foldr (++) [] (map (map constAss) code)
    pary <- return $ map (fromJust) (filter (isJust) consts)
    --liftIO $ putStrLn "new"
    --liftIO $ putStrLn $ show pary
    blocks <- return $ map (correctCode pary) code
    return blocks



copyOpt :: [[Tuple]] ->  StateT EnvMid IO [[Tuple]]
copyOpt code = do
    copies <- return $ foldr (++) [] (map (map copyAss) code)
    pary <- return $ map (fromJust) (filter (isJust) copies)
    --liftIO $ putStrLn "new"
    --liftIO $ putStrLn $ show pary
    blocks <- return $ map (correctCode pary) code
    return blocks

copyAss :: Tuple -> Maybe (Argument, Argument)
copyAss (AssOp, src, dst, _) = Just (src, dst)
copyAss _ = Nothing


correctCode :: [(Argument, Argument)] -> [Tuple] ->  [Tuple]
correctCode [] code= code
correctCode (x:xs) code =
    correctCode xs (replaceBy code x)

constExpr :: Tuple -> Tuple
constExpr (AndOp, res, ValBool i1, ValBool i2) =
    (AssOp, res, ValBool (i1 && i2), NIL)
constExpr (OrOp, res, ValBool i1, ValBool i2) =
    (OrOp, res, ValBool (i1 || i2), NIL)
constExpr (AddOp, res, ValInt i1, ValInt i2) =
    (AssOp, res, ValInt (i1 + i2), NIL)
constExpr (SubOp, res, ValInt i1, ValInt i2) =
    (AssOp, res, ValInt (i1 - i2), NIL)
constExpr (DivOp, res, ValInt i1, ValInt i2) =
    (AssOp, res, ValInt (div i1 i2), NIL)
constExpr (MulOp, res, ValInt i1, ValInt i2) =
    (AssOp, res, ValInt (i1 * i2), NIL)
constExpr (ModOp, res, ValInt i1, ValInt i2) =
    (AssOp, res, ValInt (mod i1 i2), NIL)
constExpr (NegOp, res, ValInt i1, _) =
    (AssOp, res, ValInt ((-1) *i1), NIL)
constExpr (NotOp, res, ValBool i1, _) =
    (AssOp, res, ValBool (not i1), NIL)
constExpr x = x


constAss :: Tuple -> Maybe (Argument, Argument)
constAss (AssOp, src, dst, _) =
    if(isConst dst) then Just (src, dst)
    else Nothing
constAss _ = Nothing

isConst :: Argument -> Bool
isConst (ValInt _) = True
isConst (ValBool _) = True
isConst (ValStr _) = True
isConst (ValVoid) = True
isConst _ = False
