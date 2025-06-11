module TestOperations_SyncEdit where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import qualified Vaults.Base as Base
import qualified Vaults.Operations as Operations
import qualified Vaults.Substrate as Substrate

allTests :: Test
allTests =
  TestList
    [test_syncEditSuccessful]

test_syncEditSuccessful :: Test
test_syncEditSuccessful =
  TestLabel "sync-edit successful" $
    TestCase $ do
      let operation = Operations.doSyncEditVault "remoteA" mockVaultInfo
      -- TODO There is no `git fetch` in the execResults below because `git
      -- fetch` is called with Substrate.call, not Substrate.exec. A
      -- different test is requried to assert on errors with `git fetch`.
      let mock = addMockExecResults results mockWithVaultAndRepoDir
            where
              results =
                [ D.loopSetupExec True D.remoteOp,
                  D.unlockExec True D.remoteOp,
                  D.mountExec True D.remoteOp,
                  D.loopSetupExec True D.localOp,
                  D.unlockExec True D.localOp,
                  D.mountExec True D.localOp,
                  D.gitLogExec True D.localOp,
                  D.unmountExec True D.localOp,
                  D.lockExec True D.localOp,
                  D.loopDeleteExec True D.localOp,
                  D.unmountExec True D.remoteOp,
                  D.lockExec True D.remoteOp,
                  D.loopDeleteExec True D.remoteOp
                ]
      let result = runState (runExceptT $ operation) mock
      let mockAfterExec = snd result
      assertEqual "sync-edit successful" (Right ()) (fst result)

      let expectedCommands =
            D.preOpenPartitionCmds
              ++ (D.openPartitionCmds D.remoteOp)
              ++ D.postOpenPartitionCmds D.remoteOp
              ++ [D.changeToSrcDir]
              ++ D.preOpenPartitionCmds
              ++ (D.openPartitionCmds D.localOp)
              ++ D.postOpenPartitionCmds D.localOp
              ++ [D.changeToRepoDir D.localOp]
              ++ [D.gitFetchCmd "remoteA" D.localOp]
              ++ D.preClosePartitionCmds
              ++ D.closePartitionCmds D.remoteOp
              ++ [ D.changeToRepoDir D.localOp,
                   D.setEnvCmd "VIMRUNTIME",
                   D.setEnvCmd "VIMPRIVATE",
                   D.editCmd D.localOp,
                   D.gitLogCmd
                 ]
              ++ D.preClosePartitionCmds
              ++ (D.closePartitionCmds D.localOp)
              ++ [("writeFile", ["local.log"])]

      assertEqual
        "all commands executed"
        expectedCommands
        (execRecorded mockAfterExec)
      assertAllExecsConsumed mockAfterExec
