module TestInit where

import Test.HUnit
import MockSubstrate

import Control.Monad.State
import Control.Monad.Except

import qualified Vaults.Init as Init

allTests :: Test
allTests = TestList [
    test_init
    ]

test_init :: Test
test_init = TestCase $ do
    let mock = emptyMock
    let result = runState (runExceptT $ Init.initVault "dummy" "localhost") mock
    let mockAfterExec = snd result

    assertEqual "dirs created by init"
        [ ".vault" ]
        ( createdDirs mockAfterExec)

    assertEqual "files created by init"
        [ ("", ".vault/name", "dummy")
        , ("", ".vault/local", "localhost")
        , ("",  ".vault/remotes", "")
        , ("", ".vault/remoteStore", "")
        ]
        ( writtenFiles mockAfterExec )
