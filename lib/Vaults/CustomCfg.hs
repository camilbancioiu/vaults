module Vaults.CustomCfg where

data EditOpCfg = EditCfg {
      editor :: FilePath
    , editorCLIParams :: [String]
    , autoCommitOnClose :: Bool
} deriving (Show, Read)

defaultEditCfg = EditCfg {
      editor = "nvim"
    , editorCLIParams = [ "--clean"
                        , "-c ./.config/nvim/init.vim"
                        , "." ]
    , autoCommitOnClose = True
}

