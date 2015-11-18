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
    -- special purpose registers
    lr                     :: Register m
    apsr                   :: Register m
    ipsr                   :: Register m
    epsr                   :: Register m
    primask                :: Register m
    control                :: Register m
    -- flags
    carry, overflow        :: Value
    -- arithmetic operations
    adcRegs                :: Register m -> Register m -> Value -> ComputationType m
    adcRegImm              :: Register m -> Value -> Value -> ComputationType m
    adcImms                :: Value -> Value -> Value -> ComputationType m
    adcRegMem              :: Register m -> Address -> Value -> ComputationType m
    mulRegs                :: Register m -> Register m -> ComputationType m
    -- bitwise operations
    andRegs, xorRegs       :: Register m -> Register m -> ComputationType m
    andRegImm, xorRegImm   :: Register m -> Value -> ComputationType m
    orRegImm               :: Register m -> Value -> ComputationType m
    notReg                 :: Register m -> ComputationType m
    notImm                 :: Value -> ComputationType m
    -- shifts operations
    shrRegs, shlRegs       :: Register m -> Register m -> Value -> ComputationType m
    shrRegImm, shlRegImm   :: Register m -> Value -> Value -> ComputationType m
    rorRegs, rolRegs       :: Register m -> Register m -> Value -> ComputationType m
    rorRegImm, rolRegImm   :: Register m -> Value -> Value -> ComputationType m
    -- additional functions implemented
    readMemoryBurst         :: Address -> m ()
    writeMemoryBurst        :: Address -> m ()
    bkptInstrDebugEvent     :: m ()
    updateN                 :: Value -> m () -- Negative flag
    updateZ                 :: Value -> m () -- Zero flag
    updateC                 :: Value -> m () -- Carry flag
    updateV                 :: Value -> m () -- Overflow flag
    -- Extension and conversion
    convAndExtend           :: SignType m -> Value -> Value -> Register m -> Value -> Value
    convAndExtendImm        :: Value -> Value -> Value -> Value -> Value
    currentModeIsPrivileged :: m Value
    zeroExtend              :: Value -> Register m -> Value -> Value -> m ()
    signExtend              :: Value -> Register m -> Value -> Value -> m ()
    signExtendOutVal        :: Value -> Register m -> Value -> Value -> Value
    signExtendImmOutVal     :: Value -> Value -> Value -> Value -> Value
    movBitsOutVal           :: Value -> Value -> Register m -> Value -> Value -> Value
    uInt                    :: Value -> Register m -> Value -> Value -> Value
    reverseRegister         :: ReverseType m -> Register m -> Value
    -- barrier functions
    dataMemoryBarrier                 :: Value -> m ()
    dataSynchronisationBarrier        :: Value -> m ()
    instructionSynchronizationBarrier :: Value -> m ()
    -- send event function
    hint_SendEvent          :: m ()
    callSupervisor          :: m ()

-- *****************************************************************************
-- *                          ARMv6-M Basic functions                          *
-- *****************************************************************************

-- TODO Implement this functions
updateN :: ARMv6_M m => Value -> m ()
updateN bit = do
    writeBit apsr 31 bit

updateZ :: ARMv6_M m => Value -> m ()
updateZ result = do
    bit <- isZero result --TODO implement this function
    writeBit apsr 30 bit

updateC :: ARMv6_M m => Value -> m ()
updateC bit = do
    writeBit apsr 29 bit

updateV :: ARMv6_M m => Value -> m ()
updateV bit = do
    writeBit apsr 28 bit

currentModeIsPrivileged :: ARMv6_M m => m Value
currentModeIsPrivileged = do
    return control[0] -- TODO implement this function
                      -- bit 0 represents the nPRIV flag, privilege execution
                      -- Not completely clear from the documentation
                      -- this function checks also another undefined variable

dataMemoryBarrier :: ARMv6_M m => Value -> m ()
dataMemoryBarrier option4Bits = do
    -- TODO not clear from the documentation

dataSynchronisationBarrier :: ARMv6_M m => Value -> m ()
dataSynchronisationBarrier option4Bits = do
    -- TODO not clear from the documentation

instructionSynchronizationBarrier :: ARMv6_M m => Value -> m ()
instructionSynchronizationBarrier option4Bits = do
    -- TODO not clear from the documentation

-- extract part of the register between msb and lsb bits
uInt :: ARMv6_M m => Value -> Register m -> Value -> Value -> m ()
uInt bits reg msb lsb = do
    -- TODO implement this function

-- sign/unsign/zero extension functions
zeroExtend :: ARMv6_M m => Value -> Register m -> Value -> Value -> m ()
zeroExtend bits reg msb lsb = do
    -- TODO implement this function

signExtend :: ARMv6_M m => Value -> Register m -> Value -> Value -> m ()
signExtend bits reg msb lsb = do
    -- TODO implement this function

signExtendOutVal :: ARMv6_M m => Value -> Register m -> Value -> Value -> Value
signExtendOutVal bits reg msb lsb = do
    -- TODO implement this function

signExtendImmOutVal :: ARMv6_M m => Value -> Value -> Value -> Value -> Value
signExtendImmOutVal bits imm32 msb lsb = do
    -- TODO implement this function

movBitsOutVal :: ARMv6_M m => Value -> Value -> Register m -> Value -> Value -> Value
zeroExtend msbDest lsbDest imm32 msb lsb = do
    -- TODO implement this function

-- This function reverse the order of the bytes inside a register
-- according to ReverseType type
reverseRegister :: ARMv6_M m => ReverseType m -> Register m -> m Value
reverseRegister rType reg = do
    -- TODO implement this function

-- This procedure performs a send event hint
hint_SendEvent :: ARMv6_M m => m ()
    -- TODO unclear from Specification

-- Generate exception for SVC instruction.
callSupervisor :: ARMv6_M m => m ()
    -- TODO unclear from Specification


-- *****************************************************************************
-- *                          ARMv6-M Specification                            *
-- *****************************************************************************

-- ADC (register) - Encoding T1
adc_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
adcT1 rm rdn = do
    shifted <- alu (shlRegImm rm 0 apsr[29]) -- APSR[29] = Carry flag
    result <- alu (adcRegImm rdn shifted apsr[29])
    writeRegister rdn result
    updateN result[31] --TODO Have a functionality to access any bit in a reg
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction


-- ADD (immediate) - Encoding T1
add_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
add_ImmT1 rn rd imm32  = do -- TODO imm32 should be extended from imm3 in the decoding function
    result <- alu (adcRegImm rn imm32 0)
    writeRegister rd result
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- ADD (register) - Encoding T1
add_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
add_RegT1 rm rn rd = do
    shifted <- alu (shlRegImm rm 0 apsr[29])
    result <- alu (adcRegImm rn shifted 0)
    writeRegister rd result
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- ADD (SP + immediate) - Encoding T1
add_ImmSPT1 :: ARMv6_M m => Register m -> m ()
add_RegT1 rd imm32 = do
    result <- alu (adcRegImm sp imm32 0)
    writeRegister rd result
    incAndFetchInstruction

-- ADD (SP + register) - Encoding T1
add_RegSPT1 :: ARMv6_M m => Register m -> m ()
add_RegT1 d rdm = do -- TODO d is integer representation of PC and it is computed
                     -- extending rdm
    shifted <- alu (shlRegImm rdm 0 apsr[29])
    result <- alu (adcRegImm sp shifted 0)
    if d == 15
        then writeRegister pc result
        else writeRegister rdm result
             increment pc
    fetchInstruction

-- ADR - Encoding T1
adrT1 :: ARMv6_M m => Register m -> m ()
adrT1 rd imm32 = do
    result <- alu (adcRegImm pc imm32 0)
    writeRegister rd result
    incAndFetchInstruction

-- AND (register) - Encoding T1
andT1 :: ARMv6_M m => Register m -> Register m -> m ()
andT1 rm rdn = do
    shifted <- alu (shlRegImm rm 0 apsr[29])
    result <- alu (andRegImm rdn shifted)
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- ASR (immediate) - Encoding T1
asr_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
asr_ImmT1 rm rd imm32 = do
    increment pc
    result <- alu (shrRegImm rm imm32 apsr[29])
    writeRegister rd result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- ASR (register) - Encoding T1
asr_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
asr_RegT1 rd rn rm = do
    shift_n <- convAndExtend unsigned 31 0 rm 7 0 -- TODO convert and extend a portion of register
    result  <- alu (shrRegImm rn shift_n apsr[32])
    writeRegister rd result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- B (unconditional immediate) - Encoding T1
b :: ARMv6_M m => m ()
b imm32 = do
    nextInstrLocation <- alu (adcRegImm pc imm32 0)
    writeRegister pc nextInstrLocation
    readMemory nextInstrLocation ir

-- BIC (register) - Encoding T1
bic_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
bic_RegT1 rm rdn = do
    shifted <- alu (shlRegImm rm 0 apsr[29])
    shifted_negated <- alu (notVal shifted)
    result <- alu (andRegImm rdn shifted_negated)
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- BKPT (imm) - Encoding T1
bkpt_T1 :: ARMv6_M m => m ()
bkpt_T1 imm32 = do
    bkptInstrDebugEvent
    incAndFetchInstruction

-- BL (immediate) - Encoding T1
bl_ImmT1 :: ARMv6_M m => m ()
bl_ImmT1 imm32 = do
    currentPC <- readRegister pc
    lrTmp <- convAndExtendImm currentPC 31 1 1
    writeRegister lr lrTmp
    nextInstrLocation <- alu (adcRegMem pc imm32 0)
    writeRegister pc nextInstrLocation
    readMemory nextInstrLocation ir

-- BLX (register) - Encoding T1
blx_RegT1 :: ARMv6_M m => Register m -> m ()
blx_RegT1 rm = do
    nextPC <- alu (adcRegImm pc 1 0) -- point to next instrcution
    lrTmp <- convAndExtendImm nextPC 31 1 1
    writeRegister lr lrTmp
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
    shifted <- alu (shlRegImm rm 0 apsr[29])
    result <- alu (adcRegImm rn shifted 0)
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- CMP (immediate) - Encoding T1
cmp_ImmT1 :: ARMv6_M m => Register m -> m ()
cmp_ImmT1 rn imm32 = do
    negated <- alu (notImm imm32)
    result <- alu (adcRegImm rn negated 1)
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- CMP (register) - Encoding T1
cmp_RegT1 :: ARMv6_M m => Register m -> m ()
cmp_RegT1 rn rm = do
    shifted <- alu (shlRegImm rm 0 apsr[29])
    negated <- alu (notImm shifted)
    result <- alu (adcRegImm rn negated 1)
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- CPS - Change Processor shifted_negated
cps :: ARMv6_M m => m ()
cps im = do
    if currentModeIsPrivileged
        then writeBit primask 0 im -- priority mask bit
    incAndFetchInstruction

-- CPY - alias for MOV instruction
cpy :: ARMv6_M m => Register m -> Register m -> m ()
cpy rd rn = do
    value <- readRegister rn
    writeRegister rd value
    incAndFetchInstruction

-- DMB - Encoding T1
dmb :: ARMv6_M => m ()
dmb option4Bits = do
    dataMemoryBarrier option4Bits
    incAndFetchInstruction

-- DSB - Encoding T1
dsb :: ARMv6_M => m ()
dsb option4Bits = do
    dataSynchronisationBarrier option4Bits
    incAndFetchInstruction

-- EOR (register) - Encoding T1
eor_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
eor_RegT1 rdn rm = do
    shifted <- alu (shlRegImm rm 0 apsr[29])
    result <- alu (xorRegImm rdn shifted)
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- ISB - Encoding T1
isb :: ARMv6_M => m ()
isb option4Bits = do
    instructionSynchronizationBarrier option4Bits
    incAndFetchInstruction

-- LDM - Encoding T1
ldm_T1 :: ARMv6_M m => Register m -> m ()
ldm_T1 rn = do
    memLocation <- readRegister rn
    readMemoryBurst memLocation
    incAndFetchInstruction

-- LDR (immediate) - Encoding T1
ldr_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
ldr_ImmT1 imm32 rt rn = do
    increment pc
    offset_addr = alu (adcRegImm rn imm32 0)
    readMemory offset_addr rt
    incAndFetchInstruction

-- LDR (literal: PC + Immediate) - Encoding T1
ldr_ImmPCT1 :: ARMv6_M m => Register m -> m ()
ldr_ImmPCT1 rt imm32 = do
    address = alu (adcRegImm pc imm32 0)
    readMemory address rt
    incAndFetchInstruction

-- LDR (register) - Encoding T1
ldr_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
ldr_RegT1 rt rn rm = do
    offset <- alu (shlRegImm rm 0 apsr[29])
    offset_addr <- alu (adcRegImm rn offset 0)
    readMemory offset_addr rt
    incAndFetchInstruction

-- LDRB (immediate) - Encoding T1
ldrb_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
ldrb_ImmT1 imm32 rt rn = do
    offset_addr = alu (adcRegImm rn imm32 0)
    readMemory offset_addr rt
    zeroExtend 32 rt 7 0
    incAndFetchInstruction

-- LDRB (register) - Encoding T1
ldrb_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
ldrb_RegT1 rt rn rm = do
    offset <- alu (shlRegImm rm 0 apsr[29])
    offset_addr <- alu (adcRegImm rn offset 0)
    readMemory offset_addr rt
    zeroExtend 32 rt 7 0
    incAndFetchInstruction

-- LDRH (immediate) - Encoding T1
ldrh_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
ldrh_ImmT1 imm32 rt rn = do
    offset_addr = alu (adcRegImm rn imm32 0)
    readMemory offset_addr rt
    zeroExtend 32 rt 15 0
    incAndFetchInstruction

-- LDRH (register) - Encoding T1
ldrh_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
ldrh_RegT1 rt rn rm = do
    offset <- alu (shlRegImm rm 0 apsr[29])
    offset_addr <- alu (adcRegImm rn offset 0)
    readMemory offset_addr rt
    zeroExtend 32 rt 15 0
    incAndFetchInstruction

-- LDRSB (register) - Encoding T1
ldrsb_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
ldrsb_RegT1 rt rn rm = do
    offset <- alu (shlRegImm rm 0 apsr[29])
    offset_addr <- alu (adcRegImm rn offset 0)
    readMemory offset_addr rt
    signExtend 32 rt 7 0
    incAndFetchInstruction

-- LDRSH (register) - Encoding T1
ldrsh_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
ldrsh_RegT1 rt rn rm = do
    offset <- alu (shlRegImm rm 0 apsr[29])
    offset_addr <- alu (adcRegImm rn offset 0)
    readMemory offset_addr rt
    signExtend 32 rt 15 0
    incAndFetchInstruction

-- LSL (immediate) - Encoding T1
lsl_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
lsl_ImmT1 imm5 rd rm = do
    result <- alu(shlRegImm rm imm5 apsr[29])
    writeRegister rd result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- LSL (register) - Encoding T1
lsl_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
lsl_RegT1 rdn rm = do
    shift_n <- uInt 32 rm 7 0
    result <- alu(shlRegImm rdn shift_n apsr[29])
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- LSR (immediate) - Encoding T1
lsr_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
lsr_ImmT1 imm5 rdn rm = do
    result <- alu(shrRegImm rm imm5 apsr[29])
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- LSR (register) - Encoding T1
lsr_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
lsr_RegT1 rm rdn = do
    shift_n <- uInt 32 rm 7 0
    result <- alu(shlRegImm rdn shift_n apsr[29])
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- MOV (immediate) - Encoding T1
mov_ImmT1 :: ARMv6_M m => Register m -> m ()
mov_ImmT1 rd imm32 = do
    writeRegister rd imm32
    updateN imm32[31]
    updateZ imm32
    updateC carry
    incAndFetchInstruction

-- MOV (register) - Encoding T1
mov_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
mov_RegT1 d rd rm = do
    result <- readRegister rm
    if d == 15
        then writeRegister pc result
             fetchInstruction
        else writeRegister rd result
             updateN result[31]
             updateZ result
             incAndFetchInstruction

-- MUL (register) - Encoding T1
mul_T1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
mul_T1 rdm rn = do
    result <- alu (mulRegs rdm rn)
    writeRegister rdm result
    updateN result[31]
    updateZ result
    incAndFetchInstruction

-- MVN (register) - Encoding T1
mvn_RegT1 :: ARMv6_M m => Register m -> Register m ->  m ()
mvn_RegT1 rd rm = do
    shifted <- alu (shlRegImm rm 0 apsr[29])
    result <- alu (notImm shifted)
    writeRegister rd result
    updateN result[31]
    updateZ result
    incAndFetchInstruction

-- NEG (alias for RSBS)
neg :: ARMv6_M m => Register m ->  Register m -> m ()
neg rd rm = do
    rsbs_ImmT1 rd rn 0

-- NOP already modelled inside basic architecture

-- ORR (register) - Encoding T1
orr_RegT1 ::  ARMv6_M m => Register m -> Register m -> m ()
orr_RegT1 rdn rm = do
    shifted <- alu (shlRegImm rm 0 apsr[29])
    result <- alu (orRegImm rdn shifted)
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- REV - Encoding T1
rev_T1 ::  ARMv6_M m => Register m -> Register m -> m ()
rev_T1 rd rm = do
    result <- reverseRegister word rm
    writeRegister rd result
    incAndFetchInstruction

-- REV16 - Encoding T1
rev16_T1 :: ARMv6_M m => Register m -> Register m -> m ()
rev16_T1 rd rm = do
    result <- reverseRegister hword rm
    writeRegister rd result
    incAndFetchInstruction

-- REVSH - Encoding T1
revsh_T1 :: ARMv6_M m => Register m -> Register m -> m ()
revsh_T1 rd rm = do
    result <- signExtendOutVal 32 rm 7 0
    result <- movBits 7 0 rm 15 8
    writeRegister rd result
    incAndFetchInstruction

-- ROR (register) - Encoding T1
ror_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
ror_RegT1 rm rdn = do
    shift_n <- uInt 32 rm 7 0
    result <- alu(rorRegImm rdn shift_n apsr[29])
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    incAndFetchInstruction

-- RSB  (immediate) - Encoding T1
rsb_ImmT1 :: ARMv6_M m => Register m -> Register m -> Value -> m ()
rsb_ImmT1 rn rd imm32 = do
    negatedReg <- alu (notReg rn)
    result <- alu (adcRegImm negatedReg imm32 1)
    writeRegister rd result
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- SBC (register) - Encoding T1
sbc_RegT1 :: ARMv6_M m => Register m -> Register m -> m ()
sbc_RegT1 rm rdn = do
    shifted <- alu (shlRegImm rm 0 apsr[29])
    negatedShift <- alu (notImm shifted)
    result <- alu (adcRegImm rdn negatedShift apsr[29])
    writeRegister rdn result
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- SEV - Encoding T1
sev_T1 :: ARMv6_M m => m ()
sev_T1 = do
    hint_SendEvent
    incAndFetchInstruction

-- Memory burst operation - Ldm Stm
-- Load/Store a bunch of register from/to the memory
-- STM, STMIA, STMEA - Encoding T1
stm_T1 :: ARMv6_M m => Register m -> m ()
storeBurst regBase = do
    memLocation <- readRegister regBase
    writeMemoryBurst memLocation
    incAndFetchInstruction

-- STR (immediate) - Encoding T1
str_ImmT1 :: ARMv6_M m => Value -> Register m -> Register m -> m ()
str_ImmT1 imm32 rn rt = do
    offset_addr <- alu (adcRegImm rn imm32 0)
    writeMemory offset_addr rt
    incAndFetchInstruction

-- STR (register) - Encoding T1
str_RegT1 :: ARMv6_M m => -> Register m -> Register m -> Register m -> m ()
str_RegT1 rm rn rt = do
    offset <- alu (shlRegImm rm 0 apsr[29])
    address <- alu (adcRegImm rn offset 0)
    writeMemory address rt
    incAndFetchInstruction

-- STRB (immediate) - Encoding T1
strb_ImmT1 :: ARMv6_M m => Value -> Register m -> Register m -> m ()
strb_ImmT1 imm32 rn rt = do
    offset_addr <- alu (adcRegImm rn imm32 0)
    writeMemory offset_addr rt -- TODO worth adding number of bytes to transfer
    incAndFetchInstruction

-- STRB (register) - Encoding T1
strb_RegT1 :: ARMv6_M m => Value -> Register m -> Register m -> m ()
strb_RegT1 imm32 rn rt = do
    offset <- alu (shlRegImm rm 0 apsr[29])
    address <- alu (adcRegImm rn offset 0)
    writeMemory address rt -- TODO Add number of bytes to transfer 1
    incAndFetchInstruction

-------------------------- Paulius Tuesday 17th ----------------------

-- SXTH - Encoding T1
sxth_T1 :: ARMv6_M m => Register m -> Register m -> m ()
sxth_T1 rd rm = do
    rotated   <- alu (rorRegImm rm 0 apsr[29])
    extension <- signExtendImmOutVal 32 rotated 15 0
    writeRegister rd extension
    incAndFetchInstruction

-- SXTB - Encoding T1
sxtb_T1 :: ARMv6_M m => Register m -> Register m -> m ()
sxtb_T1 rd rm = do
    rotated   <- alu (rorRegImm rm 0 apsr[29])
    extension <- signExtendImmOutVal 32 rotated 7 0
    writeRegister rd extension
    incAndFetchInstruction

-- SVC - Encoding T1
svc_T1  :: ARMv6_M m => m ()
svc_T1 imm32 = do
    callSupervisor
    incAndFetchInstruction

-- SUB (SP - immediate) - Encoding T1
sub_ImmSPT1  :: ARMv6_M m => m ()
sub_ImmSPT1  imm32 = do
    notImm32 <- alu (notImm imm32)
    result <- alu (adcRegImm sp notImm32 1)
    writeRegister sp result

-- SUB (register) - Encoding T1
sub_RegT1 :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
sub_RegT1 rm rn rd = do
    shifted     <- alu (shlRegImm rm 0 apsr[29])
    notShifted  <- alu (notImm shifted)
    result      <- alu (adcRegImm rn notShifted 1)
    writeRegister rd result
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- SUB (immediate) - Encoding T1
sub_ImmT1 :: ARMv6_M m => Register m -> Register m -> m ()
sub_ImmT1 rn rd imm32 = do
    notImm32 <- alu (notImm imm32)
    result  <- alu (adcRegImm rn notImm32 1)
    writeRegister rd result
    updateN result[31]
    updateZ result
    updateC carry
    updateV overflow
    incAndFetchInstruction

-- STRH (register) - Encoding T1
strh_RegT1  :: ARMv6_M m => Register m -> Register m -> Register m -> m ()
strh_RegT1 rm rn rt = do
    offset <- alu (shlRegImm rm 0 apsr[29])
    address <- alu (adcRegImm rn offset 0)
    writeMemory address rt -- TODO half-word
    incAndFetchInstruction

-- STRH (immediate) - Encoding T1
strh_ImmT1  :: ARMv6_M m => Register m -> Register m -> m ()
strh_ImmT1 rn rt imm32 = do
    offset_addr <- alu (adcRegImm rn imm32 0)
    writeMemory offset_adddr rt -- TODO half-word
    incAndFetchInstruction
