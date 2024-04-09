module TestUdisksctl where

import Control.Monad.State
import Control.Monad.Except

import qualified Vaults.Udisksctl as U

import Test.HUnit
import Assertions
import MockSubstrate
import qualified DummyValues as D

allTests :: Test
allTests = TestList [
      test_createLoopDevice
    , test_deleteLoopDevice
    , test_unlockDevice
    , test_mountDevice
    , test_parsingUdisksctlOutput
    ]

dummyPartition = D.partitionFile D.localOp
dummyLoopDev = D.loopDev D.localOp
dummyMapperDev = D.mapperDev D.localOp
dummyMountpoint = D.mountpoint D.localOp

test_createLoopDevice :: Test
test_createLoopDevice = TestList [
    TestLabel "udisksctl loop-setup error prevents creating loop dev" $
    TestCase $ do
        let loopSetupFail = D.loopSetupExec False D.localOp
        let mock = addMockExecResult loopSetupFail mockWithVaultDir
        let failParams = snd $ D.loopSetupCmd D.localOp
        let result = runState (runExceptT $ U.createLoopDevice dummyPartition) mock
        assertOpParamsError "loop-setup failed" failParams loopSetupFail result,

    TestLabel "udisksctl loop-setup succeeds" $
    TestCase $ do
        let loopSetupOk = D.loopSetupExec True D.localOp
        let mock = addMockExecResult loopSetupOk mockWithVaultDir
        let result = runState (runExceptT $ U.createLoopDevice dummyPartition) mock
        assertEqual "loop-setup success" (Right dummyLoopDev) (fst result)
    ]

test_unlockDevice :: Test
test_unlockDevice = TestList [
    TestLabel "udisksctl unlock error prevents unlocking" $
    TestCase $ do
        let unlockFail = D.unlockExec False D.localOp
        let mock = addMockExecResult unlockFail mockWithVaultDir
        let failParams = snd $ D.unlockCmd D.localOp
        let result = runState (runExceptT $ U.unlockDevice dummyLoopDev) mock
        assertOpParamsError "unlock failed" failParams unlockFail result,

    TestLabel "udisksctl unlock succeeds" $
    TestCase $ do
        let unlockOk = D.unlockExec True D.localOp
        let mock = addMockExecResult unlockOk mockWithVaultDir
        let result = runState (runExceptT $ U.unlockDevice dummyLoopDev) mock
        assertEqual "unlock succeeds" (Right dummyMapperDev) (fst result)
    ]

test_deleteLoopDevice :: Test
test_deleteLoopDevice = TestList [
    TestLabel "udisksctl loop-delete succeeds" $
    TestCase $ do
        let loopDeleteOk = D.loopDeleteExec True D.localOp
        let mock = addMockExecResult loopDeleteOk mockWithVaultDir
        let result = runState (runExceptT $ U.deleteLoopDevice dummyLoopDev) mock
        assertEqual "loop-delete succeeds" (Right ()) (fst result)
    ]

test_mountDevice :: Test
test_mountDevice = TestList [
    -- TODO test case for mounting an already mounted device
    TestLabel "udisksctl mount succeeds" $
    TestCase $ do
        let mountOk = D.mountExec True D.localOp
        let mock = addMockExecResult mountOk mockWithVaultDir
        let result = runState (runExceptT $ U.mountDevice dummyMapperDev) mock
        assertEqual "mount succeeds" (Right dummyMountpoint) (fst result)
    ]

test_unmountDevice :: Test
test_unmountDevice = TestList [
    -- TODO test case for unmounting an unmounted device
    TestLabel "udisksctl unmount succeeds" $
    TestCase $ do
        let unmountOk = D.unmountExec True D.localOp
        let mock = addMockExecResult unmountOk mockWithVaultDir
        let result = runState (runExceptT $ U.unmountDevice dummyMapperDev) mock
        assertEqual "unmount succeeded" (Right ()) (fst result)
    ]

test_parsingUdisksctlOutput :: Test
test_parsingUdisksctlOutput = TestList [
    TestLabel "parsing output of loop-setup" $
    TestCase $ do
        let output = ""
        U.parseOutputLoopSetup output @?= U.invalidOutput
        let output = "Mapped dummy.vault as /dev/loop42."
        U.parseOutputLoopSetup output @?= U.invalidOutput
        let output = "Mapped file dummy.vault as ."
        U.parseOutputLoopSetup output @?= U.invalidOutput
        let output = "Mapped file dummy.vault as /dev/lo.op42."
        U.parseOutputLoopSetup output @?= U.invalidOutput
        let output = "Mapped file dummy.vault as /dev/loop42"
        U.parseOutputLoopSetup output @=? U.invalidOutput
        let output = "Mapped file dummy.vault as /dev/loop42."
        U.parseOutputLoopSetup output @=? (Right "/dev/loop42"),

    TestLabel "parsing output of unlock" $
    TestCase $ do
        let output = ""
        U.parseOutputUnlock output @?= U.invalidOutput
        let output = "Unlocked /dev/loop42 as /dev/dm-4"
        U.parseOutputUnlock output @?= U.invalidOutput
        let output = "Unlocked /dev/loop42 as /dev/dm-4."
        U.parseOutputUnlock output @?= (Right "/dev/dm-4"),

    TestLabel "parsing output of mount" $
    TestCase $ do
        let output = ""
        U.parseOutputMount output @?= U.invalidOutput
        let output = "Mounted /dev/dm-4 as /mnt/point."
        U.parseOutputMount output @?= U.invalidOutput
        let output = "Mounted /dev/dm-4 as /mnt/this.point."
        U.parseOutputMount output @?= U.invalidOutput
        let output = "Mounted /dev/dm-4 as /mnt/this.point"
        U.parseOutputMount output @?= (Right "/mnt/this.point")
        let output = "Mounted /dev/dm-4 as /mnt/point"
        U.parseOutputMount output @?= (Right "/mnt/point")

    ]
