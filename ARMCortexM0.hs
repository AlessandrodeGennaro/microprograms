{-# LANGUAGE TypeFamilies #-}

-- NOTES
-- In this implementation of the ARMv6-M 11 groups of instructions are present.

module Microprogram (
	module Control.Monad,
    Microprogram (..), Address (..), Value (..),
    increment, fetchArgument, fetchNextOpcode, load, store, jump
    ) where

import Control.Monad

-- Type synonyms let us avoid the pain of converting values to addresses, yet 
-- improve readability
type Address = Int
type Value   = Int

-- Enumerated types for computational unit
-- data ComputationType = Add | Sub | Mul | Div | Shl | Shr deriving (Enum)

class Monad m => Microprogram m where
    data Register m
    data ComputationType m
    data MemoryOperation m
    pc, ir, sp            :: Register m
    add, sub              :: ComputationType m
    store, load           :: MemoryOperation m
    burstLoad, burstStore :: MemoryOperation m
    memoryUnit            :: Address -> Register m -> MemoryOperation m -> m ()
    readRegister          :: Register m -> m Value
    writeRegister         :: Register m -> Value -> m ()
    alu                   :: Register m -> Register m | Address -> ComputationType m -> m Value
    --readMemory    :: Address -> m Value
    --writeMemory   :: Address -> Value -> m ()

-- Increment the value stored in a register
increment :: Microprogram m => Register m -> m ()
increment register = do
    value <- readRegister register
    writeRegister register (value + 1) -- TODO: change flags

-- Decrement the value stored in a register
decrement :: Microprogram m => Register m -> m ()
decrement register = do
    value <- readRegister register
    writeRegister register (value - 1) -- TODO: change flags

-- Increment the program counter and fetch the address where the offset is stored
fetchAddressImmediate :: Microprogram m => m Address
fetchAddressImmediate = do
    increment pc
    readRegister pc

-- Fetch the next instruction opcode and store it in the opcode register
incAndFetchInstruction :: Microprogram m => m ()
incAndFetchInstruction = do
    addressNextInstruction <- fetchAddressImmediate
    memoryUnit <- addressNextInstruction ir load

-- Fetch the next instruction opcode and store it in the opcode register
fetchInstruction :: Microprogram m => m ()
fetchInstruction = do
    addressNextInstruction <- readRegister pc
    memoryUnit <- addressNextInstruction ir load

-- Pop operation. The value is stored into a register
pop :: Microprogram m => Register m -> m Value
pop register = do
    stackAddress <- readRegister sp
    memoryUnit stackAddress register load
    decrement sp

-- Push operation. The value is stored into th
push :: Microprogram m => Register m -> m Value
push register = do
    stackAddress <- readRegister sp
    memoryUnit stackAddress register store
    increment sp

--
-- 11 POs model of the ARMv6-M
--

-- Unconditional branch - #123 to PC Branch - (PCIU -> IFU -> ALU -> IFU2)
uncBranch :: Microprogram m => m ()
uncBranch = do
    addressOffset <- fetchAddressImmediate
    addressNextInst <- alu pc addressOffset add
    memoryUnit addressNextInst ir load

-- Arithmetic operations - #123 to Rn - (PCIU -> IFU -> PCIU2 -> IFU2 IFU -> ALU -> IFU2)
-- Operation between a register and an immediate.
arithOps :: Microprogram m => Register m -> ComputationType m -> m ()
arithOps register cType = do
    addressImmediate <- fetchAddressImmediate
    result <- alu register addressImmediate cType
    writeRegister register result
    incAndFetchInstruction

-- Arithmetic operation - Rn to Rn - (PCIU -> IFU ALU)
-- Operation between two registers
rnToRn :: Microprogram m => Register m -> Register m -> ComputationType m -> m ()
rnToRn register1 register2 cType = do
    result <- alu register1 register2 cType
    writeRegister register1 result
    incAndFetchInstruction

-- Branch operation - Rn to PC (ALU -> IFU)
-- Address of the branch contained inside a register
rnToPc :: Microprogram m => Register m -> m ()
rnToPC register = do
    addressNextInst <- alu pc register add
    writeRegister pc addressNextInst
    fetchInstruction

-- Load/Store operations - Ldr Str Imm - (PCIU -> IFU -> ALU -> MAU -> PCIU2 -> IFU2)
-- Load & store where target = register + offset
-- TODO: (PCIU -> IFU -> ALU -> MAU IFU -> PCIU2 -> IFU2) ?
ldrStrImm :: Microprogram m => register m -> MemoryOperation m -> m ()
ldrStrImm register baseRegister mOp = do
    addressOffset <- fetchAddressImmediate
    addressMemory <- alu baseRegister addressOffset add
    value <- readRegister register
    memoryUnit addressMemory value mOp
    incAndFetchInstruction

-- Memory burst operation - Ldm Stm - (PCIU -> IFU MAU)
-- Load/Store a bunch of register from/to the memory
ldmStm :: Microprogram m => Register m -> Register m -> MemoryOperation m -> m ()
ldmStm baseRegister mOp = do
    addressMem <- readRegister baseRegister
    memoryUnit addressMem R 0 mOp
    incAndFetchInstruction

-- Load/Store register addressing mode - Str Ldr Reg Pop - (PCIU -> IFU ALU -> MAU)
strLdrRegPop :: Microprogram m => Register m -> Register m -> Register m -> MemoryOperation m -> m ()
strLdrRegPop regDest regBase regOffset mOp = do
    memAddress <- alu regBase regOffset add
    memoryUnit memAddress regDest mOp
    incAndFetchInstruction

-- Pop - Pop PC - (MAU -> IFU)
-- TODO: If PC is not the target of the pop, where is the PC incremented?
-- As it is done, I assume to model a POP into the Program Counter
popPc :: Microprogram m => m ()
popPc = do
    pop pc
    fetchInstruction

-- No Operation - Nop - (PCIU -> PCIU2 -> IFU)
-- TODO: why are two PCIU needed?
nop :: Microprogram m => m ()
nop = do
    fetchNextInstruction

-- Branch from memory - Ldr Reg PC - (ALU -> MAU -> IFU) 
-- Address taken from the memory (register addressing mode)
branchMemory :: MicroProgram m => Register m -> Register m -> m ()
branchMemory regBase regOffset = do
    memLocation <- alu regBase regOffset add
    memoryUnit memLocation pc load
    fetchInstruction

-- Branch from memory - Ldr Reg PC - (ALU -> MAU -> IFU) 
-- Address taken from the memory (immediate addressing mode)
branchMemory :: MicroProgram m => Register m -> Register m -> m ()
branchMemory regBase = do
    addressOffset <- fetchAddressImmediate
    memLocation <- alu regBase addressOffset add
    memoryUnit memLocation pc load
    fetchInstruction
