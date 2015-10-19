{-# LANGUAGE TypeFamilies #-}

module Simulation (
    Processor (..), Simulation (..), Register (..), ComputationType(..),
    simulate) where

import Control.Monad
import ARMCortexM0_v2
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

data Processor = Processor { memory :: IntMap Value, registerBank :: IntMap Value }

lookupRegister :: Processor -> Register Simulation -> Value
lookupRegister (Processor _ regs) r =
    Map.findWithDefault 0 (registerID r) regs

updateRegister :: Processor -> Register Simulation -> Value -> Processor
updateRegister (Processor mem regs) r v =
    Processor mem $ Map.insert (registerID r) v regs

lookupMemory :: Processor -> Address -> Value
lookupMemory (Processor mem _) a =
    Map.findWithDefault 0 a mem

updateMemory :: Processor -> Address -> Value -> Processor
updateMemory (Processor mem regs) a v =
    Processor (Map.insert a v mem) regs

updateMemoryMulti :: Processor -> [(Address, Value)] -> Processor
updateMemoryMulti = foldr (\(a, v) p -> updateMemory p a v)

updateRegisterMulti :: Processor -> [(Register Simulation, Value)] -> Processor
updateRegisterMulti = foldr (\(r, v) p -> updateRegister p r v)

copyRegistersToMemory :: Processor -> Address -> Processor
copyRegistersToMemory p addr = updateMemoryMulti p (zip [addr..addr+7] values)
  where
    values = map (lookupRegister p . R) [0..7]

copyMemoryToRegisters :: Processor -> Address -> Processor
copyMemoryToRegisters p addr = updateRegisterMulti p (zip regs values)
  where
    regs   = map R [0..7]
    values = map (lookupMemory p) [addr..addr+7]

instance Show Processor where
    show (Processor mem regs) = "Memory = " ++ show mem ++ "\nRegister bank = "
                                ++ show regs ++ "\n"

data Simulation a = Simulation (Processor -> (a, Processor))

simulate :: Simulation a -> Processor -> (a, Processor)
simulate (Simulation f) processor = f processor

instance Functor Simulation where
    fmap f (Simulation s) = Simulation $ \p ->
                              let (a, p') = s p
                              in (f a, p')

instance Applicative Simulation where
   pure  = return
   (<*>) = ap

instance Monad Simulation where
    return a = Simulation $ \p -> (a, p)
    Simulation f >>= g = Simulation $ \p ->
                          let
                            (a, p') = f p
                            Simulation h = g a
                          in h p'

instance Microprogram_v2 Simulation where
    data Register Simulation = R Int | RegisterPC | RegisterIR | RegisterSP
    data ComputationType Simulation =
          AddRegs (Register Simulation) (Register Simulation)
        | SubRegs (Register Simulation) (Register Simulation)
        | AddRegMem (Register Simulation) Address
        | SubRegMem (Register Simulation) Address
        | AddRegImm (Register Simulation)
        | SubRegImm (Register Simulation)
        | IncReg (Register Simulation)
        | DecReg (Register Simulation)
    -- standard registers
    pc         = RegisterPC
    ir         = RegisterIR
    sp         = RegisterSP
    addRegs    = AddRegs
    -- operations with one register
    incReg     = IncReg
    decReg     = DecReg

    -- read a register and return the value stored
    readRegister register = Simulation $ \p -> (lookupRegister p register, p)

    -- write a valure into a register
    writeRegister register value = Simulation $
        \p -> ((), updateRegister p register value)

    -- emulate alu behaviour
    alu cType = Simulation $
        \p -> case (cType) of
            -- REG1 + REG2 operation
            (AddRegs reg1 reg2) -> ((lookupRegister p reg1) +
                                    (lookupRegister p reg2), p)
            -- REG1 - REG2 operation
            (SubRegs reg1 reg2) -> ((lookupRegister p reg1) -
                                    (lookupRegister p reg2), p)
            -- REG + MEM operation
            (AddRegMem reg1 addr) -> ((lookupRegister p reg1) +
                                    (lookupMemory p addr), p)
            -- REG - MEM operation
            (SubRegMem reg1 addr) -> ((lookupRegister p reg1) -
                                    (lookupMemory p addr), p)
            -- REG + IMM operation
            (AddRegImm reg) -> ((lookupRegister p reg) +
                            (lookupMemory p (lookupRegister p pc)), p)
            -- REG - IMM operation
            (SubRegImm reg) -> ((lookupRegister p reg) -
                            (lookupMemory p (lookupRegister p pc)), p)
            -- REG increment operation
            (IncReg reg) -> ((lookupRegister p reg) + 1,p)
            -- REG decrement operation
            (DecReg reg) -> ((lookupRegister p reg) - 1,p)

    -- store memory operation
    writeMemory addr reg = Simulation $
        \p @ (Processor mem regs) ->
            ((), updateMemory p addr $ lookupRegister p reg)


    -- load memory operation
    readMemory addr reg = Simulation $
        \p @ (Processor mem regs) ->
            ((), updateRegister p reg $ lookupMemory p addr)

instance ARMCortexM0_v2 Simulation where
    -- operations between registers
    subRegs    = SubRegs
    -- operations between register and memory (2nd operand)
    addRegMem     = AddRegMem
    subRegMem     = SubRegMem
    -- operations between register and immediate (inside IR reg)
    addRegImm     = AddRegImm
    subRegImm     = SubRegImm

    -- store memory burst (R0-7) operation
    writeMemoryBurst addr = Simulation $
        \p -> ((), copyRegistersToMemory p addr)

    -- load memory burst (R0-7) operation
    readMemoryBurst addr = Simulation $
        \p -> ((), copyMemoryToRegisters p addr)


registerID :: Register Simulation -> Int
registerID (R n)          = n
registerID RegisterPC     = -1
registerID RegisterIR     = -2
registerID RegisterSP     = -3
