{-# LANGUAGE DeriveDataTypeable #-}
module MonadAnn.SrcInfo where

import Data.Generics (Data)

-- | A simple location in a file
type Loc = (FilePath, Int, Int) -- file, line and column

-- | The source information of a do statement
data SrcInfo = Info (Maybe String) (Maybe Loc)
  deriving (Show, Read, Eq, Ord)

-- | A tag constructor for SrcInfo that can be used with annotation pragmas
data SrcInfoName = SrcInfo
  deriving Data
