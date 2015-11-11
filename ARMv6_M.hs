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
    subRegs, adcRegs      :: Register m -> Register m -> ComputationType m
    addRegMem, subRegMem  :: Register m -> Address -> ComputationType m
    addRegImm, subRegImm  :: Register m -> ComputationType m
    readMemoryBurst       :: Address -> m ()
    writeMemoryBurst      :: Address -> m ()

-- *****************************************************************************
-- *                          ARMv6-M Specification                            *
-- *****************************************************************************

-- ADC (register) - Encoding T1
adc_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
adcT1 rm rdn = do
    shifted <- alu (shlReg rm 0)
    writeRegister rm shifted
    result <- alu (adcRegs rm rdn)
    writeRegister rdn result
    incAndFetchInstruction
