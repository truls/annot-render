{-# LANGUAGE DeriveGeneric #-}
module Config
  ( parseConfig
  , Config (..)
  ) where

import           GHC.Generics

import           Data.ByteString.Char8
import           Data.Yaml

data Config = Config
  { assignment           :: String
  , author               :: String
  , annotBoxWidth        :: Int
  , lineAdjustmentFactor :: Float
  } deriving Generic

instance FromJSON Config

parseConfig :: ByteString -> Maybe Config
parseConfig bs = decode bs :: Maybe Config
