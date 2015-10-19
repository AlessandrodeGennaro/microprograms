{-# LANGUAGE TypeFamilies #-}

module Simulation (Processor (..), Simulation (..), Register (..),
                    ComputationType (..), simulate) where

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

updateMemoryBurst :: Processor -> Address -> Processor
updateMemoryBurst p addr =
    Map.insert (addr)   (lookupRegister p (r 0)) mem
    Map.insert (addr+1) (lookupRegister p (r 1)) mem
    Map.insert (addr+2) (lookupRegister p (r 2)) mem
    Map.insert (addr+3) (lookupRegister p (r 3)) mem
    Map.insert (addr+4) (lookupRegister p (r 4)) mem
    Map.insert (addr+5) (lookupRegister p (r 5)) mem
    Map.insert (addr+6) (lookupRegister p (r 6)) mem
    Map.insert (addr+7) (lookupRegister p (r 7)) mem
    Processor mem regs

updateRegisterBurst :: Processor -> Address -> Processor
updateRegisterBurst (Processor mem regs) addr =
    Map.insert (r 0) (Map.findWithDefault 0 (addr) mem) regs
    Map.insert (r 1) (Map.findWithDefault 0 (addr+1) mem) regs
    Map.insert (r 2) (Map.findWithDefault 0 (addr+2) mem) regs
    Map.insert (r 3) (Map.findWithDefault 0 (addr+3) mem) regs
    Map.insert (r 4) (Map.findWithDefault 0 (addr+4) mem) regs
    Map.insert (r 5) (Map.findWithDefault 0 (addr+5) mem) regs
    Map.insert (r 6) (Map.findWithDefault 0 (addr+6) mem) regs
    Map.insert (r 7) (Map.findWithDefault 0 (addr+7) mem) regs
    (Processor mem regs)

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

--instance Applicative Simulation where
--    pure a = Simulation $ \p -> (a, p)
--    Simulation f <*> Simulation a = Simulation $ \p ->
--                                      let (g, p') = f p
--                                          (b, p'') = a p'
--                                      in (g b, p'')

instance Monad Simulation where
    --return = pure
    Simulation f >>= g = Simulation $ \p ->
                          let
                            (a, p') = f p
                            Simulation h = g a
                          in h p'

instance ARMCortexM0_v2 Simulation where
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
    -- operations between registers
    addRegs    = AddRegs
    subRegs    = SubRegs
    -- operations between register and memory (2nd operand)
    addRegMem     = AddRegMem
    subRegMem     = SubRegMem
    -- operations between register and immediate (inside IR reg)
    addRegImm     = AddRegImm
    subRegImm     = SubRegImm
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

    -- store memory burst (R0-7) operation
    writeMemoryBurst addr = Simulation $
        \p -> updateMemoryBurst p addr

    -- load memory burst (R0-7) operation
    readMemoryBurst addr = Simulation $
        \p -> updateRegisterBurst p addr


registerID :: Register Simulation -> Int
registerID (R n)          = n
registerID RegisterPC     = -1
registerID RegisterIR     = -2
registerID RegisterSP     = -3
