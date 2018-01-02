module FrontEndErrors where

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
import System.Exit ( exitFailure, exitSuccess)

import Environment
import TypesStuff
import Misc

frontError :: IO () -> StateT Env IO ()
frontError kind = do
    liftIO kind
    liftIO exitFailure

mainUndeclared :: IO ()
mainUndeclared = liftIO $ putStrLn "Can't find function int main()"

classNameDupErr :: Debug -> Type Debug -> StateT Env IO ()
classNameDupErr info1 info2 = do
        liftIO $ putStrLn $ (show $ fromJust info1) ++
            ": Duplicated name of class " ++ "name" ++
            ", firstly declared at: " ++ (show $ getTypeInfo info2)
        liftIO exitFailure

classFieldDupErr :: Debug -> Type Debug -> Ident -> StateT Env IO ()
classFieldDupErr info1 info2 ident = do
    liftIO $ putStrLn $ (show $ fromJust info1) ++ ": Duplicated name of class's field " ++ (show ident) ++
        ", firstly declared at: " ++ (show $ getTypeInfo info2)
    liftIO $ exitFailure

varUndeclared :: Debug -> Ident -> StateT Env IO (TypeD)
varUndeclared debug name = do
    liftIO $ putStrLn $ (show $ fromJust debug) ++ ": variable " ++ (show name) ++" doesn't exists"
    liftIO exitFailure

operatorErr :: Bool -> TypeD -> Debug -> StateT Env IO (TypeD)
operatorErr multi type_ debug = do
    if(multi) then
        liftIO $ putStrLn $ (show $ fromJust debug) ++ ": " ++ (show type_) ++  " values needed in that operation"
    else
        liftIO $ putStrLn $ (show $ fromJust debug) ++ ": " ++ (show type_) ++  " value needed in that operation"
    liftIO exitFailure

compError :: Debug -> StateT Env IO (TypeD)
compError debug = do
    liftIO $ putStrLn $ (show $ fromJust debug) ++ ": Comparision between unmatching types"
    liftIO exitFailure

funUndeclared :: Debug -> String -> StateT Env IO (TypeD)
funUndeclared info name = do
    liftIO $ putStrLn $ (show $ fromJust info) ++ ": function " ++ name ++ "doesn't exists"
    liftIO exitFailure

numberOfArgsErr :: Debug -> String -> StateT Env IO (TypeD)
numberOfArgsErr info name = do
    liftIO $ putStrLn $ (show  $ fromJust info) ++ ": Wrong number of arguments given for " ++ name
    liftIO exitFailure

typesOfArgsErr :: Debug -> String -> StateT Env IO (TypeD)
typesOfArgsErr info name = do
    liftIO $ putStrLn $ (show  $ fromJust info) ++ ": Wrong types of arguments given for function " ++ name
    liftIO exitFailure
