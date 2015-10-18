import ARMCortexM0
import Simulation

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

program :: Simulation ()
program = do

    -- read/write register test
    writeRegister (R 0) 100
    value <- readRegister (R 0)
    increment (R 0)
    writeRegister (R 1) value
    writeRegister (R 2) 500

    -- memoryUnit store test
    memoryUnit 0 (R 2) store

    -- aluRegs test
    value2 <- alu $ addRegs (R 0) (R 1)
    writeRegister (R 3) value2

    -- aluMem test
    value3 <- alu $ addMem (R 0) 0
    writeRegister (R 4) value3

    -- memoryUnit load test
    memoryUnit 0 (R 6) load


-- *****************************************************************************
-- *                        Test ARMv6-M instructions                          *
-- *****************************************************************************
armBranchImmediate :: Simulation ()
armBranchImmediate = do

    let programAddress = 1000
    let offset = 20
    let nextInstruction = 9999

    -- unconditional branch: PC = PC + IMM
    writeRegister pc programAddress

    writeRegister (R 31) offset
    memoryUnit (programAddress + 1) (R 31) store

    writeRegister (R 32) nextInstruction
    memoryUnit (programAddress + 1 + offset) (R 32) store

    uncBranch

-- Arithmetic operation: Reg = Reg + Imm
armArithRegImm :: Simulation ()
armArithRegImm = do
    let programAddress = 1000

    writeRegister pc programAddress
    writeRegister (R 31) 111
    writeRegister (R 32) 100
    memoryUnit (programAddress + 1) (R 32) store

    arithOpsImm (R 60) (addImm (R 31))


armArithRegs :: Simulation ()
armArithRegs = do
    let programAddress = 1000

    -- Arithmetic operation: Reg1 = Reg1 + Reg2
    writeRegister pc programAddress
    writeRegister (R 31) 333
    writeRegister (R 32) 233

    arithOpsReg (R 60) (addRegs (R 31) (R 32))

armBranchReg :: Simulation ()
armBranchReg = do
    let programAddress = 1000
    let offset = 20
    let nextInstruction = 9999

    -- Unconditional Branch: PC = PC + REG
    writeRegister pc programAddress

    writeRegister (R 31) offset

    writeRegister (R 32) nextInstruction
    memoryUnit (programAddress + offset) (R 32) store

    branchReg (R 31)

armMemRegImm :: Simulation ()
armMemRegImm = do
    let programAddress = 1000
    let targetMemory = 30

    -- Memory Operation: RegDest = LOAD/STORE[ RegBase + Immediate ]
    writeRegister pc programAddress

    writeRegister (R 32) 666
    memoryUnit targetMemory (R 32) store
    writeRegister (R 32) 14
    memoryUnit (programAddress + 1) (R 32) store
    writeRegister (R 32) 16
    memoryImm (R 31) (R 32) load

armMemRegs :: Simulation ()
armMemRegs = do

    let programAddress = 1000
    let targetMemory = 30

    -- Memory Operation: RegDest = LOAD/STORE[ RegBase + RegOffset ]
    writeRegister pc programAddress

    writeRegister (R 32) 888
    memoryUnit targetMemory (R 32) store
    writeRegister (R 32) 14
    writeRegister (R 33) 16

    memoryReg (R 31) (R 32) (R 33) load



processor = Processor (Map.empty) (Map.empty)
armCortexM0 = Processor (Map.empty) (Map.empty)

main = do
    putStrLn $ "\nTest Basic functions:\n"
    putStrLn $ ">Initial processor:\n" ++ show processor
    let (value, newProcessor) = simulate program processor
    putStrLn $ ">New processor:\n" ++ show newProcessor

    putStrLn $ "Test ARMv6-M instructions.\n"
    putStrLn $ "1) Unconditional Branch immediate;"
    putStrLn $ "2) Arithmetic operation: Reg = Reg + Imm;"
    putStrLn $ "3) Arithmetic operation: Reg1 = Reg1 + Reg2;"
    putStrLn $ "4) Unconditional Branch: PC = PC + REG;"
    putStrLn $ "5) Memory Operation: RegDest = LOAD/STORE[ RegBase + Immediate ];"
    putStrLn $ "6) Memory Operation: RegDest = LOAD/STORE[ RegBase + RegOffset ];\n"
    putStrLn $ "Insert the number of the instruction you want to simulate: "
    number' <- getLine
    let number = read number' :: Int

    putStrLn $ ">Initial processor:\n" ++ show armCortexM0
    let (value, newArmCortexM0) = case (number) of
            1 -> simulate armBranchImmediate armCortexM0
            2 -> simulate armArithRegImm armCortexM0
            3 -> simulate armArithRegs armCortexM0
            4 -> simulate armBranchReg armCortexM0
            5 -> simulate armMemRegImm armCortexM0
            6 -> simulate armMemRegs armCortexM0
    putStrLn $ ">New processor:\n" ++ show newArmCortexM0
