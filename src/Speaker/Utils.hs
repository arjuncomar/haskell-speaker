module Speaker.Utils
    (  toByteString
    ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L

toByteString :: Show s => s -> L.ByteString
toByteString = B.toLazyByteString . B.stringUtf8 . show
