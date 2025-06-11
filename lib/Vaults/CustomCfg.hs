module Vaults.CustomCfg where

data EditOpCfg = EditCfg
  { editor :: FilePath,
    editorCLIParams :: [String],
    envVars :: [(String, String)],
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
      envVars =
        [ ("VIMRUNTIME", ".config/nvim"),
          ("VIMPRIVATE", "1")
        ],
      autoCommitOnClose = True
    }
