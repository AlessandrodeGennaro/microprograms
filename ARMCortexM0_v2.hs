{-# LANGUAGE TypeFamilies #-}

-- NOTES
-- In this implementation of the ARMv6-M 11 groups of instructions are present.

module ARMCortexM0_v2 (
    --module Control.Monad,
    ARMCortexM0_v2 (..), Microprogram_v2 (..), Register (..), ComputationType (..),
    Address, Value,
    -- instructions available
    uncBranch, branchReg, storeImm, loadImm, storeBurst, loadBurst, arithOpsImm,
    arithOpsReg, storeReg, loadReg, branchPop, nop, branchMemReg, branchMemImm
    ) where

--import Control.Monad
import Microprogram_v2

class Microprogram_v2 m => ARMCortexM0_v2 m where
    subRegs               :: Register m -> Register m -> ComputationType m
    addRegMem, subRegMem  :: Register m -> Address -> ComputationType m
    addRegImm, subRegImm  :: Register m -> ComputationType m
    readMemoryBurst       :: Address -> m ()
    writeMemoryBurst      :: Address -> m ()

-- *****************************************************************************
-- *               ARMv6-M Specification (14 instructions)                     *
-- *****************************************************************************

-- Unconditional branch - #123 to PC Branch - (PCIU -> IFU -> ALU -> IFU2)
uncBranch :: ARMCortexM0_v2 m => m ()
uncBranch = do
    offsetLocation <- fetchAddressImmediate
    nextInstrLocation <- alu (addRegMem pc offsetLocation)
    writeRegister pc nextInstrLocation
    readMemory nextInstrLocation ir

-- Arithmetic operations - #123 to Rn - (PCIU -> IFU -> PCIU2 -> IFU2 IFU -> ALU -> IFU2)
-- Operation between a register and an immediate.
arithOpsImm :: ARMCortexM0_v2 m => Register m -> ComputationType m -> m ()
arithOpsImm regDest cType = do
    increment pc
    result <- alu cType
    writeRegister regDest result
    incAndFetchInstruction

-- Arithmetic operation - Rn to Rn - (PCIU -> IFU ALU)
-- Operation between two registers
arithOpsReg :: ARMCortexM0_v2 m => Register m -> ComputationType m -> m ()
arithOpsReg regDest cType = do
    result <- alu cType
    writeRegister regDest result
    incAndFetchInstruction

-- Branch operation - Rn to PC (ALU -> IFU)
-- Address of the branch contained inside a register
branchReg :: ARMCortexM0_v2 m => Register m -> m ()
branchReg regOffset = do
    nextInstrLocation <- alu (addRegs pc regOffset)
    writeRegister pc nextInstrLocation
    fetchInstruction

-- Load/Store operations - Ldr Str Imm
-- Load & store where target = register + offset
storeImm :: ARMCortexM0_v2 m => Register m -> Register m -> m ()
storeImm regDest regBase = do
    offsetLocation <- fetchAddressImmediate
    memLocation <- alu (addRegMem regBase offsetLocation)
    writeMemory memLocation regDest
    incAndFetchInstruction

loadImm :: ARMCortexM0_v2 m => Register m -> Register m -> m ()
loadImm regDest regBase = do
    offsetLocation <- fetchAddressImmediate
    memLocation <- alu (addRegMem regBase offsetLocation)
    readMemory memLocation regDest
    incAndFetchInstruction

-- Memory burst operation - Ldm Stm
-- Load/Store a bunch of register from/to the memory
storeBurst :: ARMCortexM0_v2 m => Register m -> m ()
storeBurst regBase = do
    memLocation <- readRegister regBase
    writeMemoryBurst memLocation
    incAndFetchInstruction

loadBurst :: ARMCortexM0_v2 m => Register m -> m ()
loadBurst regBase = do
    memLocation <- readRegister regBase
    readMemoryBurst memLocation
    incAndFetchInstruction

-- Load/Store register addressing mode - Str Ldr Reg Pop
storeReg :: ARMCortexM0_v2 m => Register m -> Register m -> Register m -> m ()
storeReg regDest regBase regOffset = do
    memLocation <- alu (addRegs regBase regOffset)
    writeMemoryBurst memLocation
    incAndFetchInstruction

loadReg :: ARMCortexM0_v2 m => Register m -> Register m -> Register m -> m ()
loadReg regDest regBase regOffset = do
    memLocation <- alu (addRegs regBase regOffset)
    readMemory memLocation regDest
    incAndFetchInstruction

-- Pop - Pop PC - (MAU -> IFU)
-- As it is done, I assume to model a POP into the Program Counter
branchPop :: ARMCortexM0_v2 m => m ()
branchPop = do
    pop pc
    fetchInstruction

-- No Operation - Nop - (PCIU -> PCIU2 -> IFU)
-- TODO: why are two PCIU needed in the old implementation?
nop :: ARMCortexM0_v2 m => m ()
nop = do
    incAndFetchInstruction

-- Branch from registers - Ldr Reg PC - (ALU -> MAU -> IFU)
-- Address taken from the memory (register addressing mode)
branchMemReg :: ARMCortexM0_v2 m => Register m -> Register m -> m ()
branchMemReg regBase regOffset = do
    memTargetJump <- alu (addRegs regBase regOffset)
    readMemory memTargetJump pc
    fetchInstruction

-- Branch from memory - Ldr Imm PC - (PCIU -> IFU -> ALU -> MAU -> IFU2)
-- Address taken from the memory (immediate addressing mode)
branchMemImm :: ARMCortexM0_v2 m => Register m -> m ()
branchMemImm regBase = do
    offsetLocation <- fetchAddressImmediate
    memTargetJump <- alu (addRegMem regBase offsetLocation)
    readMemory memTargetJump pc
    fetchInstruction
