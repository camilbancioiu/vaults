module TestUdisksctl where

import Test.HUnit
import Control.Monad.State
import Control.Monad.Except

import Assertions
import MockSubstrate

import qualified Vaults.Substrate as Sub
import Vaults.Udisksctl

allTests :: Test
allTests = TestList [
      test_createLoopDevice
    , test_deleteLoopDevice
    , test_unlockDevice
    , test_mountDevice
    , test_parsingUdisksctlOutput
    ]

test_createLoopDevice :: Test
test_createLoopDevice = TestList [
    TestLabel "udisksctl loop-setup error prevents creating loop dev" $
    TestCase $ do
        let mock = addMockExecResult loopSetupFail mockWithVaultDir
        let result = runState (runExceptT $ createLoopDevice "/what") mock
        assertOpError "loop-setup failed" result,

    TestLabel "udisksctl loop-setup succeeds" $
    TestCase $ do
        let mock = addMockExecResult loopSetupOk mockWithVaultDir
        let result = runState (runExceptT $ createLoopDevice "dummy.vault") mock
        assertEqual "loop-setup success" (Right "/dev/loop42") (fst result)
    ]

test_unlockDevice :: Test
test_unlockDevice = TestList [
    TestLabel "udisksctl unlock error prevents unlocking" $
    TestCase $ do
        let mock = addMockExecResult loopSetupFail mockWithVaultDir
        let result = runState (runExceptT $ unlockDevice "what") mock
        assertOpError "unlock failed" result,

    TestLabel "udisksctl unlock succeeds" $
    TestCase $ do
        let mock = addMockExecResult unlockOk mockWithVaultDir
        let result = runState (runExceptT $ unlockDevice "what") mock
        assertEqual "unlock succeeds" (Right "/dev/dm-4") (fst result)
    ]

test_deleteLoopDevice :: Test
test_deleteLoopDevice = TestList [
    TestLabel "udisksctl loop-delete succeeds" $
    TestCase $ do
        let mock = addMockExecResult loopDeleteOk mockWithVaultDir
        let result = runState (runExceptT $ deleteLoopDevice "/dev/loop42") mock
        assertEqual "loop-delete succeeds" (Right ()) (fst result)
    ]

test_mountDevice :: Test
test_mountDevice = TestList [
    TestLabel "udisksctl mount succeeds" $
    TestCase $ do
        let mock = addMockExecResult mountOk mockWithVaultDir
        let result = runState (runExceptT $ mountDevice "/dev/dm-4") mock
        assertEqual "mount succeeds" (Right "/mnt/point") (fst result)
    ]

test_parsingUdisksctlOutput :: Test
test_parsingUdisksctlOutput = TestList [
    TestLabel "parsing output of loop-setup" $
    TestCase $ do
        let output = ""
        parseOutputLoopSetup output @?= invalidOutput
        let output = "Mapped dummy.vault as /dev/loop42."
        parseOutputLoopSetup output @?= invalidOutput
        let output = "Mapped file dummy.vault as ."
        parseOutputLoopSetup output @?= invalidOutput
        let output = "Mapped file dummy.vault as /dev/lo.op42."
        parseOutputLoopSetup output @?= invalidOutput
        let output = "Mapped file dummy.vault as /dev/loop42"
        parseOutputLoopSetup output @=? invalidOutput
        let output = "Mapped file dummy.vault as /dev/loop42."
        parseOutputLoopSetup output @=? (Right "/dev/loop42"),

    TestLabel "parsing output of unlock" $
    TestCase $ do
        let output = ""
        parseOutputUnlock output @?= invalidOutput
        let output = "Unlocked /dev/loop42 as /dev/dm-4"
        parseOutputUnlock output @?= invalidOutput
        let output = "Unlocked /dev/loop42 as /dev/dm-4."
        parseOutputUnlock output @?= (Right "/dev/dm-4"),

    TestLabel "parsing output of mount" $
    TestCase $ do
        let output = ""
        parseOutputMount output @?= invalidOutput
        let output = "Mounted /dev/dm-4 as /mnt/point."
        parseOutputMount output @?= invalidOutput
        let output = "Mounted /dev/dm-4 as /mnt/point"
        parseOutputMount output @?= (Right "/mnt/point")

    ]
