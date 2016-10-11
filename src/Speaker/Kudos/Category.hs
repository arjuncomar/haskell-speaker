{-# LANGUAGE TemplateHaskell #-}
module Speaker.Kudos.Category where

import Database.Persist.TH
import Control.Lens
import Data.Aeson.TH

data Category = Teamwork | Improvement | Delivery | Experiment
  deriving (Show, Read, Eq)
derivePersistField "Category"
makePrisms ''Category
$(deriveJSON defaultOptions ''Category)
