module OutputType (OutputType(..)) where

data OutputType = Html
                | Console
  deriving Show

instance Read OutputType where
  readsPrec _ "html" = [(Html, "")]
  readsPrec _ "console" = [(Console, "")]
  readsPrec _ _ = []
