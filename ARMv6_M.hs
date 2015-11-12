{-# LANGUAGE TypeFamilies #-}

-- NOTES
-- In this implementation of the ARMv6_M 11 groups of instructions are present.

module ARMv6_M (
    --module Control.Monad,
    ARMv6_M (..), Microprogram_v2 (..), Register (..), ComputationType (..),
    Address, Value,
    -- instructions available
    uncBranch, branchReg, storeImm, loadImm, storeBurst, loadBurst, arithOpsImm,
    arithOpsReg, storeReg, loadReg, branchPop, nop, branchMemReg, branchMemImm
    ) where

--import Control.Monad
import Microprogram_v2

class Microprogram_v2 m => ARMv6_M m where
    -- registers
    lr                     :: Register m
    -- arithmetic operations
    subRegs, adcRegs       :: Register m -> Register m -> ComputationType m
    shrRegs, shlRegs       :: Register m -> Register m -> ComputationType m
    shrReg, shlReg         :: Register m -> Value -> ComputationType m
    andRegs                :: Register m -> Register m -> ComputationType m
    notReg                 :: Register m -> ComputationType m
    addRegMem, subRegMem   :: Register m -> Address -> ComputationType m
    addRegImm, subRegImm   :: Register m -> ComputationType m
    addRegValue, subRegVal :: Register m -> Value -> ComputationType m
    shrRegImm, shlRegImm   :: Register m -> ComputationType m
    -- functions implemented
    readMemoryBurst        :: Address -> m ()
    writeMemoryBurst       :: Address -> m ()
    bkptInstrDebugEvent    :: m ()

-- *****************************************************************************
-- *                          ARMv6-M Specification                            *
-- *****************************************************************************

-- Memory burst operation - Ldm Stm
-- Load/Store a bunch of register from/to the memory
storeBurst :: ARMv6_M m => Register m -> m ()
storeBurst regBase = do
    memLocation <- readRegister regBase
    writeMemoryBurst memLocation
    incAndFetchInstruction

loadBurst :: ARMv6_M m => Register m -> m ()
loadBurst regBase = do
    memLocation <- readRegister regBase
    readMemoryBurst memLocation
    incAndFetchInstruction


-- ADC (register) - Encoding T1
adc_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
adcT1 rm rdn = do
    shifted <- alu (shlReg rm 0)
    writeRegister rm shifted
    result <- alu (adcRegs rm rdn)
    writeRegister rdn result
    incAndFetchInstruction


-- ADD (immediate) - Encoding T1
add_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
add_ImmT1 rn rd = do
    increment pc
    result <- alu (addRegImm rn)
    writeRegister rd result
    incAndFetchInstruction

-- ADD (register) - Encoding T1
add_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
add_RegT1 rm rn rd = do
    shifted <- alu (shlReg rm 0)
    writeRegister rm shifted
    result <- alu (addRegs rm rn)
    writeRegister rd result
    incAndFetchInstruction

-- ADD (SP + immediate) - Encoding T1
add_ImmSPT1 :: ARMv6_M m => Register m -> m ()
add_RegT1 rd = do
    increment pc
    result <- alu (addRegImm sp)
    writeRegister rd result
    incAndFetchInstruction

-- ADD (SP + register) - Encoding T1
add_RegSPT1 :: ARMv6_M m => Register m -> m ()
add_RegT1 rdm = do
    shifted <- alu (shlReg rdm 0)
    writeRegister rdm shifted
    result <- alu (addRegs sp rdm)
    writeRegister rdm result
    incAndFetchInstruction

-- ADR - Encoding T1
adrT1 :: ARMv6_M m => Register m -> m ()
adrT1 rd = do
    increment pc
    result <- alu (addRegImm pc)
    writeRegister rd result
    incAndFetchInstruction

-- AND (register) - Encoding T1
andT1 :: ARMv6_M m => Register m -> Register m -> m ()
andT1 rm rdn = do
    shifted <- alu (shl rm 0)
    writeRegister rm shifted
    result <- alu (andRegs rdn rm)
    writeRegister rdn result
    incAndFetchInstruction

-- ASR (immediate) - Encoding T1
asr_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
asr_ImmT1 rm rd = do
    increment pc
    result <- alu (shrRegImm rm)
    writeRegister rd result
    incAndFetchInstruction

-- ASR (register) - Encoding T1
asr_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
asr_RegT1 rd rn rm = do
    result  <- alu (shrRegs rm rn)
    writeRegister rd result
    incAndFetchInstruction

-- B (unconditional immediate) - Encoding T1
b :: ARMv6_M m => m ()
b = do
    offsetLocation <- fetchAddressImmediate
    nextInstrLocation <- alu (addRegMem pc offsetLocation)
    writeRegister pc nextInstrLocation
    readMemory nextInstrLocation ir

-- BIC (register) - Encoding T1
bic_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
bic_RegT1 rm rdn = do
    shifted <- alu (shlReg rm 0)
    writeRegister rm shifted
    shifted_negated <- alu (notReg rm)
    writeRegister rm shifted_negated
    result <- alu (andRegs rdn rm)
    writeRegister rdn result
    incAndFetchInstruction

-- BKPT (imm) - Encoding T1
bkpt_T1 :: ARMv6_M m => m ()
bkpt_T1 = do
    increment pc
    bkptInstrDebugEvent
    incAndFetchInstruction

-- BL (immediate) - Encoding T1
bl_ImmT1 :: ARMv6_M m => m ()
bl_ImmT1 = do
    currentPC <- readRegister pc
    writeRegister lr currentPC
    offsetLocation <- fetchAddressImmediate
    nextInstrLocation <- alu (addRegMem pc offsetLocation)
    writeRegister pc nextInstrLocation
    readMemory nextInstrLocation ir

-- BLX (register) - Encoding T1
blx_RegT1 :: ARMv6_M m => Register m -> m ()
blx_RegT1 rm = do
    currentPC <- readRegister pc
    writeRegister lr currentPC
    increment lr -- link to next instruction
    nextInstrLocation <- alu (addRegs pc rm)
    writeRegister pc nextInstrLocation
    readMemory nextInstrLocation ir

-- BX (register) - Encoding T1
bx_RegT1 :: ARMv6_M m => Register m -> m ()
bx_RegT1 rm = do
    nextInstrLocation <- alu (addRegs pc rm)
    writeRegister pc nextInstrLocation
    readMemory nextInstrLocation ir

-- CMN (register) - Encoding T1
cmn_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
cmn_RegT1 rn rm = do
    shifted <- alu (shlReg rm 0)
    result <- alu (addRegValue rm shifted) -- alu is supposed to update the flags
    incAndFetchInstruction

-- CMP (immediate) - Encoding T1
cmp_ImmT1 :: ARMv6_M m => Register m -> m ()
cmp_ImmT1 rn = do
    increment pc
    result <- alu (subRegImm rn)
    incAndFetchInstruction

-- CMP (register) - Encoding T1
cmp_RegT1 :: ARMv6_M m => Register m -> m ()
cmp_RegT1 rn rm = do
    shifted <- alu (shlReg rm 0)
    result <- alu (subRegVal rn shifted)
    incAndFetchInstruction

-- CPY
cpy :: ARMv6_M m => Register m -> Register m -> m ()
cpy rd rn = do
    value <- readRegister rn
    writeRegister rd value
    incAndFetchInstruction
