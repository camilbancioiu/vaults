module Vaults.CustomCfg where

data EditOpCfg = EditCfg {
      editor :: FilePath
    , editorCLIParams :: [String]
    , autoCommitOnClose :: Bool
} deriving (Show, Read)

defaultEditCfg = EditCfg {
      editor = "nvim"
    , editorCLIParams = [ "--clean"
                        , "." ]
    , autoCommitOnClose = True
}
