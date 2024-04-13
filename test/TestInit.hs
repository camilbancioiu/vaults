module TestInit where

allTests :: Test
allTests = TestList [
    test_init
]

test_init :: Test
test_init = do
    let mock = emptyMock
    let result = runState (runExceptT $ initVault "dummy" "local") mock
    let mockAfterExec = snd result

    
