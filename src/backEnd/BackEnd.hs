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

{-
data RegName = EAX | EBX | ECX | EDX | ESP | EBP | ESI | EDI
    deriving (Eq, Ord, Show, Read)
data RegAsm = Register Int --EAX | EBX | ECX | EDX | ESP | EBP | ESI | EDI
    deriving (Eq, Ord, Show, Read)
data Location = Stack Int | Memory Int | RegLoc RegAsm
    deriving (Eq, Ord, Show, Read)

data AsmOperand = RegOp RegName | MemOp Int | ValOp Int

type RegMap = M.Map RegAsm [Argument]
type AddrMap = M.Map Argument [Location]
type Code = [AsmInstr]
type StackPtr = Int

type EnvBack = (Code, RegMap, AddrMap, [RegAsm], StackPtr)
initialEnvBack = ([], M.empty, M.empty, [], 0)

data AsmInstr =
      Mov Int AsmOperand AsmOperand
    | Sub AsmOperand AsmOperand
    | Push AsmOperand
    | Pop AsmOperand
    | Add AsmOperand AsmOperand
    | LabAsm String


generateAsm :: [FunctionCode]  -> IO ()
generateAsm p =  (liftIO $ evalStateT (backEnd p) initialEnvBack)

backEnd :: [FunctionCode] -> StateT EnvBack IO ()
backEnd graph = do
    put([], M.fromList [(Register 0, []),(Register 1, []), (Register 2, [])],
        M.empty, [Register 0, Register 1, Register 2], 0)
    liftIO $ putStrLn "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
    mapM (generateFun) graph
    return ()

generateFun :: FunctionCode -> StateT EnvBack IO ()
generateFun ((Ident name), start_block, blocks, size) = do
    putInstr(LabAsm name)
    (Just (first_block, _)) <- return $ M.lookup start_block blocks
    args <- return $ mapArgs 0 first_block
    liftIO $ putStrLn name
    liftIO $ putStrLn $ show size
    putInstr(Push (RegOp EBP))
    putInstr(Mov 32 (RegOp ESP) (RegOp EBP))
    putInstr(Sub (ValOp size) (RegOp ESP))
    return ()

putInstr :: AsmInstr -> StateT EnvBack IO ()
putInstr instr = do
    (code, regs, addres, free, ptr) <- get
    put(code++[instr], regs, addres, free, ptr)

findEmptyReg :: StateT EnvBack IO (Maybe RegAsm)
findEmptyReg = do
    (code, regs, addres, free, ptr) <- get
    if(length free > 0) then do
        put(code, regs, addres, tail free, ptr)
        return $ Just (head free)
    else return (Nothing)

freeRegister :: StateT EnvBack IO (RegAsm)
freeRegister = do
    (code, regs, addres, free, ptr) <- get
    --putInstr (Push $ RegLoc (Register 0))
    return $ (Register 0)

getReg :: Tuple -> StateT EnvBack IO Location
getReg (op, x, y, z) = do
    emptyReg <- findEmptyReg
    case(emptyReg) of
        (Just reg) -> return $ RegLoc reg
        Nothing -> do
            return $ Stack 0

mapArgs :: Int -> [Tuple] -> Int
mapArgs x [] = x
mapArgs x ((ARGS, NIL, NIL, NIL):xs) = x
mapArgs x ((Alloca t, param, NIL, NIL):xs) =
    mapArgs (x+ (typeSize t)) xs
mapArgs s (x:xs) = mapArgs (s+1) xs

pushStack :: Type (Maybe (Int, Int)) -> StateT EnvBack IO (Int)
pushStack t = do
    (code, regs, addres, free, ptr) <- get
    put(code, regs, addres, free, ptr - (typeSize t))
    return $ ptr - (typeSize t)
-}
