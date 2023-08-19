module TestVaults where

import Test.HUnit

import qualified Vaults
import Substrate

allTests :: Test
allTests = TestList [
    test_loadVault
    ]

dummySubstrate :: Substrate
dummySubstrate = undefinedSubstrate {
    readFileSub = dummyReadFile
    }

dummyReadFile :: FilePath -> IO String
dummyReadFile ".vault/name" = return "dummy"
dummyReadFile ".vault/local" = return "local"
dummyReadFile ".vault/remotes" = return "remoteA\nremoteB"
dummyReadFile ".vault/remoteStore" = return "remoteStore"

test_loadVault :: Test
test_loadVault = TestCase $ do
    v <- Vaults.loadVault dummySubstrate
    (Vaults.name v) @?= "dummy"
    (Vaults.localname v) @?= "local"
    (Vaults.remotes v) @?= ["remoteA", "remoteB"]
    (Vaults.remoteStore v) @?= "remoteStore"
