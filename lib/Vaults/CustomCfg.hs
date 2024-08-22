module Vaults.CustomCfg where

data EditOpCfg = EditCfg
  { editor :: FilePath,
    editorCLIParams :: [String],
    envVar :: (String, String),
    autoCommitOnClose :: Bool
  }
  deriving (Show, Read)

defaultEditCfg =
  EditCfg
    { editor = "nvim",
      editorCLIParams =
        [ "--clean",
          "--cmd",
          "source .config/nvim/init.vim",
          "."
        ],
      envVar = ("VIMRUNTIME", ".config/nvim"),
      autoCommitOnClose = True
    }
