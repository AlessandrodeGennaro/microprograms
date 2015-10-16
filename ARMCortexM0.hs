{-# LANGUAGE TypeFamilies #-}

-- NOTES
-- In this implementation of the ARMv6-M 11 groups of instructions are present.

module ARMCortexM0 (
	module Control.Monad,
    ARMCortexM0 (..), Address (..), Value (..),
    -- basic operations
    increment, decrement, fetchAddressImmediate, incAndFetchInstruction, 
    fetchInstruction, pop, push,
    -- instructions available
    uncBranch, arithOpsImm, arithOpsReg, branchReg, memoryImm, memoryBurst,
    memoryReg, branchPop, nop, branchMemReg, branchMemImm
    ) where

import Control.Monad

-- Type synonyms let us avoid the pain of converting values to addresses, yet 
-- improve readability
type Address = Int
type Value   = Int

-- Enumerated types for computational unit
-- data ComputationType = Add | Sub | Mul | Div | Shl | Shr deriving (Enum)

class Monad m => ARMCortexM0 m where
    data Register m
    data ComputationType m
    data MemoryOperation m
    pc, ir, sp, dummy     :: Register m
    addRegs, addMem       :: ComputationType m
    store, load           :: MemoryOperation m
    burstLoad, burstStore :: MemoryOperation m
    memoryUnit            :: Address -> Register m -> MemoryOperation m -> m ()
    readRegister          :: Register m -> m Value
    writeRegister         :: Register m -> Value -> m ()
    alu                   :: Register m -> Register m -> Address -> ComputationType m -> m Value
    --readMemory    :: Address -> m Value
    --writeMemory   :: Address -> Value -> m ()

-- Increment the value stored in a register
increment :: ARMCortexM0 m => Register m -> m ()
increment register = do
    value <- readRegister register
    writeRegister register (value + 1) -- TODO: change flags

-- Decrement the value stored in a register
decrement :: ARMCortexM0 m => Register m -> m ()
decrement register = do
    value <- readRegister register
    writeRegister register (value - 1) -- TODO: change flags

-- Increment the program counter and fetch the address where the offset is stored
fetchAddressImmediate :: ARMCortexM0 m => m Address
fetchAddressImmediate = do
    increment pc
    readRegister pc

-- Fetch the next instruction opcode and store it in the opcode register
fetchInstruction :: ARMCortexM0 m => m ()
fetchInstruction = do
    addressNextInstruction <- readRegister pc
    memoryUnit addressNextInstruction ir load

-- Fetch the next instruction opcode and store it in the opcode register
incAndFetchInstruction :: ARMCortexM0 m => m ()
incAndFetchInstruction = do
    addressNextInstruction <- fetchAddressImmediate
    memoryUnit addressNextInstruction ir load

-- Pop operation. The value is stored into a register
pop :: ARMCortexM0 m => Register m -> m ()
pop register = do
    spAddress <- readRegister sp
    memoryUnit spAddress register load
    decrement sp

-- Push operation. The value is stored into th
push :: ARMCortexM0 m => Register m -> m ()
push register = do
    stackAddress <- readRegister sp
    memoryUnit stackAddress register store
    increment sp

-- *****************************************************************************
-- *                      11 POs model of the ARMv6-M                          *
-- *****************************************************************************

-- Unconditional branch - #123 to PC Branch - (PCIU -> IFU -> ALU -> IFU2)
uncBranch :: ARMCortexM0 m => m ()
uncBranch = do
    offsetLocation <- fetchAddressImmediate
    nextInstrLocation <- alu pc dummy offsetLocation addMem
    memoryUnit nextInstrLocation ir load

-- Arithmetic operations - #123 to Rn - (PCIU -> IFU -> PCIU2 -> IFU2 IFU -> ALU -> IFU2)
-- Operation between a register and an immediate.
arithOpsImm :: ARMCortexM0 m => Register m -> ComputationType m -> m ()
arithOpsImm redDest cType = do
    immLocation <- fetchAddressImmediate
    result <- alu redDest dummy immLocation cType
    writeRegister redDest result
    incAndFetchInstruction

-- Arithmetic operation - Rn to Rn - (PCIU -> IFU ALU)
-- Operation between two registers
arithOpsReg :: ARMCortexM0 m => Register m -> Register m -> ComputationType m -> m ()
arithOpsReg regDest regOp cType = do
    result <- alu regDest regOp 0 cType
    writeRegister regDest result
    incAndFetchInstruction

-- Branch operation - Rn to PC (ALU -> IFU)
-- Address of the branch contained inside a register
branchReg :: ARMCortexM0 m => Register m -> m ()
branchReg regOffset = do
    nextInstrLocation <- alu pc regOffset 0 addRegs
    writeRegister pc nextInstrLocation
    fetchInstruction

-- Load/Store operations - Ldr Str Imm - (PCIU -> IFU -> ALU -> MAU -> PCIU2 -> IFU2)
-- Load & store where target = register + offset
-- TODO: (PCIU -> IFU -> ALU -> MAU IFU -> PCIU2 -> IFU2) ?
memoryImm :: ARMCortexM0 m => Register m -> Register m -> MemoryOperation m -> m ()
memoryImm regDest regBase mOp = do
    offsetLocation <- fetchAddressImmediate
    memLocation <- alu regBase dummy offsetLocation addMem
    memoryUnit memLocation regDest mOp
    incAndFetchInstruction

-- Memory burst operation - Ldm Stm - (PCIU -> IFU MAU)
-- Load/Store a bunch of register from/to the memory
memoryBurst :: ARMCortexM0 m => Register m -> MemoryOperation m -> m ()
memoryBurst regBase mOp = do
    memLocation <- readRegister regBase
    memoryUnit memLocation sp mOp -- TODO sp represents a dummy register in this case
    incAndFetchInstruction

-- Load/Store register addressing mode - Str Ldr Reg Pop - (PCIU -> IFU ALU -> MAU)
memoryReg :: ARMCortexM0 m => Register m -> Register m -> Register m -> MemoryOperation m -> m ()
memoryReg regDest regBase regOffset mOp = do
    memLocation <- alu regBase regOffset 0 addRegs
    memoryUnit memLocation regDest mOp
    incAndFetchInstruction

-- Pop - Pop PC - (MAU -> IFU)
-- TODO: If PC is not the target of the pop, where is the PC incremented?
-- As it is done, I assume to model a POP into the Program Counter
branchPop :: ARMCortexM0 m => m ()
branchPop = do
    pop pc
    fetchInstruction

-- No Operation - Nop - (PCIU -> PCIU2 -> IFU)
-- TODO: why are two PCIU needed in the old implementation?
nop :: ARMCortexM0 m => m ()
nop = do
    incAndFetchInstruction

-- Branch from registers - Ldr Reg PC - (ALU -> MAU -> IFU) 
-- Address taken from the memory (register addressing mode)
branchMemReg :: ARMCortexM0 m => Register m -> Register m -> m ()
branchMemReg regBase regOffset = do
    memTargetJump <- alu regBase regOffset 0 addRegs
    memoryUnit memTargetJump pc load
    fetchInstruction

-- Branch from memory - Ldr Imm PC - (PCIU -> IFU -> ALU -> MAU -> IFU2) 
-- Address taken from the memory (immediate addressing mode)
branchMemImm :: ARMCortexM0 m => Register m -> m ()
branchMemImm regBase = do
    offsetLocation <- fetchAddressImmediate
    memTargetJump <- alu regBase dummy offsetLocation addMem
    memoryUnit memTargetJump pc load
    fetchInstruction
