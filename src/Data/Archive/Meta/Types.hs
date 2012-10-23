{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Data.Archive.Meta.Types where
import qualified Data.Aeson as A
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.ByteString.Lazy
import Data.Text
import Control.Applicative ((<$>), (<*>))
import Data.Attoparsec.Number
import Data.Aeson.Types (Parser)
import Control.Lens
import Control.Arrow
import Control.Monad

data Context = Context {
        _value :: Value,
        _files :: [File]
    }
    deriving(Show, Eq)

data File = File {
        _filePath  :: Text,
        _fileBytes :: ByteString 
    }
    deriving(Show, Eq)
    
data Value = Object              !Object 
           | Array               !Array  
           | String              !Text   
           | Number              !Number 
           | Bool                !Bool   
           | Null                 
           | InternalReference   !Text   
           | ExternalReference   { 
                _referenceType :: !Text,
                _ref           :: !Text 
            }
             deriving (Eq, Show)
             
type Object = HashMap Text Value
type Array  = Vector Value
             
instance A.ToJSON Value where
  toJSON e = case e of
    Object x -> A.Object . fmap A.toJSON $ x
    Array  x -> A.Array  . fmap A.toJSON $ x
    String x -> A.String x
    Number x -> A.Number x
    Bool x   -> A.Bool x
    Null     -> A.Null
    InternalReference path -> A.object ["__type__" A..= ("InternalReference" :: Text),
                                        "ref"      A..= path]
    ExternalReference typ path -> A.object 
        ["__type__"       A..= ("ExternalReference" :: Text),
         "referencedType" A..= typ,
         "ref"            A..= path ]

objectOrSpecial :: Object -> Parser Value
objectOrSpecial o = 
    case H.lookup "__type__" o of
        Just x 
            | x == String "InternalReference" -> InternalReference <$> getValue o "ref"
            | x == String "ExternalReference" -> ExternalReference <$> getValue o "referencedType" 
                                                                   <*> getValue o "ref"
            | otherwise -> fail $ "bad __type__ " ++ show x
        Nothing -> return . Object $ o
                     
mstrength :: Monad m => (a, m b) -> m (a, b)
mstrength (x, y) = (\b -> (x, b)) `liftM` y

getValue :: Object -> Text -> Parser Text
getValue obj key = case H.lookup key obj of
               Nothing -> fail $ "key " ++ show key ++ " not present"
               Just (String v)  -> return v
               Just x -> fail $ "value for key " ++ show key ++ "was not text"
                                            
instance A.FromJSON Value where
    parseJSON e = case e of
        A.Object x -> objectOrSpecial <=< fmap H.fromList . sequence . 
                            fmap (mstrength . second A.parseJSON) . H.toList $ x
        A.Array  x -> fmap (Array . V.fromList) . sequence . fmap A.parseJSON . V.toList $ x    
        A.String x -> return . String $ x
        A.Number x -> return . Number $ x        
        A.Bool x   -> return . Bool $ x 
        A.Null     -> return Null
               
makeLenses  ''File        
        
        
