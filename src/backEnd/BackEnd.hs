module BackEnd where


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
import IntermediateEnv


data RegAsm = Register Int --EAX | EBX | ECX | EDX | ESP | EBP | ESI | EDI
data Location = Stack Int | Memory Int | RegLoc RegAsm

type RegMap = M.Map RegAsm [Argument]
type AddrMap = M.Map Argument [Location]
type MemMap = M.Map Int [Argument]
type Code = [AsmInstr]

type EnvBack = (Code, RegMap, AddrMap, MemMap, [RegAsm], [RegAsm])
initialEnvBack = ([], M.empty, M.empty, M.empty, [], [])

data AsmInstr =
      Mov Location Location
    | Push Location
    | Pop Location
    | Add Location Location
    | Sub Location Location


generateAsm :: [FunctionCode]  -> IO ()
generateAsm p =  (liftIO $ evalStateT (backEnd p) initialEnvBack)

backEnd :: [FunctionCode] -> StateT EnvBack IO ()
backEnd graph = do
    liftIO $ putStrLn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX111"

putInstr :: AsmInstr -> StateT EnvBack IO ()
putInstr instr = do
    (code, regs, addres, mem, free, occupied) <- get
    put(code++[instr], regs, addres, mem, free, occupied)

findEmptyReg :: StateT EnvBack IO (Maybe RegAsm)
findEmptyReg = do
    (code, regs, addres, mem, free, occupied) <- get
    if(length free > 0) then do
        put(code, regs, addres, mem, tail free, occupied ++ [head free])
        return $ Just (head free)
    else return (Nothing)

freeRegister :: StateT EnvBack IO (RegAsm)
freeRegister = do
    (code, regs, addres, mem, free, occupied) <- get
    putInstr (Push (RegLoc (head occupied)))
    return $ ( (head occupied))

getReg :: Tuple -> StateT EnvBack IO Location
getReg (op, x, y, z) = do
    emptyReg <- findEmptyReg
    case(emptyReg) of
        (Just reg) -> return $ RegLoc reg
        Nothing -> do
            return $ Stack 0
