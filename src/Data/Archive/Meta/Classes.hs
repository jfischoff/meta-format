{-# LANGUAGE OverloadedStrings #-}
module Data.Archive.Meta.Classes where
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Archive.Meta.Types
import Data.ByteString.Lazy (ByteString)
import Codec.Archive.Tar.Entry (toTarPath, fileEntry, Entry)
import Codec.Archive.Tar(write)
import Data.Text (unpack)
import qualified Data.Aeson as A
import Control.Lens
import Control.Arrow

class ToContext a where
    toContext :: a -> Context
    
instance ToContext Context where
    toContext = id
    
class ToValue a where
    toValue :: a -> Value
    
instance ToValue Context where
    toValue = _value
    
instance ToValue Value where
    toValue = id

--TODO profile if it is faster to first convert to a vector and then map
instance ToValue a => ToValue [a] where
    toValue = Array . V.fromList . map toValue
    
class ToTar a where
    toTar :: a -> Either String [Entry]
    
class ToFiles a where
    toFiles :: a -> [File]

instance ToFiles Context where
    toFiles (Context v fs) = File "manifest" (A.encode v) : fs 
    
mstrengthL = fmap swap . mstrength . swap

swap (x, y) = (y, x)
    
createFileEntry :: File -> Either String Entry
createFileEntry = fmap (uncurry fileEntry) . mstrengthL . first (toTarPath False . unpack) . 
                    fromFile

defaultToTar :: (ToFiles a, ToValue a) => a -> Either String [Entry]
defaultToTar = mapM createFileEntry . toFiles       

instance ToTar Context where
    toTar = defaultToTar

encode :: ToTar a => a -> Either String ByteString 
encode = fmap write . toTar

fromFile (File x y) = (x, y)
