module Tree
  ( File(..)
  , Expr(..)
  ) where

import Data.Text (Text)
import Data.Aeson ((.:), withObject, FromJSON(parseJSON))

data File = File
  { fileName :: Text
  , fileExpression :: Expr
  , fileLocation :: Location
  } deriving (Show)

data Location = Location
  { locationStart :: Int
  , locationEnd :: Int
  , locationFilename :: Text
  } deriving (Show)

data Param = Param
  { paramText :: Text
  , paramLocation :: Location
  } deriving (Show)

data Expr
  = ELet
      { name :: Param
      , value :: Expr
      , next :: Expr
      , location :: Location
      }
  | EFunction
      { parameters :: [Param]
      , value :: Expr
      , location :: Location
      }
  | EIf
      { condition :: Expr
      , thenBranch :: Expr
      , elseBranch :: Expr
      , location :: Location
      }
  | EBinary
      { lhs :: Expr
      , op :: Text
      , rhs :: Expr
      , location :: Location
      }
  | EVar
      { text :: Text
      , location :: Location
      }
  | ECall
      { callee :: Expr
      , arguments :: [Expr]
      , location :: Location
      }
  | EInt
      { intValue :: Int
      , location :: Location
      }
  | EPrint
      { value :: Expr
      , location :: Location
      }
  deriving (Show)

instance FromJSON File where
  parseJSON = withObject "File" $ \v ->
    File <$> v .: "name" <*> v .: "expression" <*> v .: "location"

instance FromJSON Location where
  parseJSON = withObject "Location" $ \v ->
    Location <$> v .: "start" <*> v .: "end" <*> v .: "filename"

instance FromJSON Param where
  parseJSON = withObject "Param" $ \v ->
    Param <$> v .: "text" <*> v .: "location"

instance FromJSON Expr where
  parseJSON = withObject "Expr" $ \v -> do
    (kind :: Text) <- v .: "kind"
    case kind of
      "Let" -> ELet <$> v .: "name" <*> v .: "value" <*> v .: "next" <*> v .: "location"
      "If" -> EIf <$> v .: "condition" <*> v .: "then" <*> v .: "otherwise" <*> v .: "location"
      "Binary" -> EBinary <$> v .: "lhs" <*> v .: "op" <*> v .: "rhs" <*> v .: "location"
      "Var" -> EVar <$> v .: "text" <*> v .: "location"
      "Call" -> ECall <$> v .: "callee" <*> v .: "arguments" <*> v .: "location"
      "Function" -> EFunction <$> v .: "parameters" <*> v .: "value" <*> v .: "location"
      "Int" -> EInt <$> v .: "value" <*> v .: "location"
      "Print" -> EPrint <$> v .: "value" <*> v .: "location"
      _ -> fail "Unknown kind"
