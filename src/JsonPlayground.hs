{-# LANGUAGE DeriveAnyClass #-}

module JsonPlayground where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data Person
  = Person1 (Either String String) String String
  | Person2 (Either String String) String String
  -- data Person = Person
  --   { firstName :: String
  --   , lastName  :: String
  --   }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON)

data Person'
  = Person1' {boss :: Maybe String, firstName :: String, lastName :: String}
  | Person2' {boss :: Maybe String, firstName :: String, lastName :: String}
  -- data Person = Person
  --   { firstName :: String
  --   , lastName  :: String
  --   }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON)

-- our fields are snake_case instead
-- instance ToJSON Person where
--   toJSON (Person { firstName = firstName, lastName = lastName }) =
--     object [ "first_name" .= firstName
--            , "last_name"  .= lastName
--            ]

-- >>> encode (Person1 (Left "Jack") "Karl" "Popper")
-- >>> encode (Person1 (Right "Samuel") "Karl" "Popper")
-- >>> encode (Person2 (Left "Jack") "Karl" "Popper")
-- >>> encode (Person2 (Right "Samuel") "Karl" "Popper")
-- "{\"contents\":[{\"Left\":\"Jack\"},\"Karl\",\"Popper\"],\"tag\":\"Person1\"}"
-- "{\"contents\":[{\"Right\":\"Samuel\"},\"Karl\",\"Popper\"],\"tag\":\"Person1\"}"
-- "{\"contents\":[{\"Left\":\"Jack\"},\"Karl\",\"Popper\"],\"tag\":\"Person2\"}"
-- "{\"contents\":[{\"Right\":\"Samuel\"},\"Karl\",\"Popper\"],\"tag\":\"Person2\"}"

-- >>> encode (Person1' (Just "Jack") "Karl" "Popper")
-- >>> encode (Person1' Nothing "Karl" "Popper")
-- >>> encode (Person2' (Just "Jack") "Karl" "Popper")
-- >>> encode (Person2' Nothing "Karl" "Popper")
-- "{\"boss\":\"Jack\",\"firstName\":\"Karl\",\"lastName\":\"Popper\",\"tag\":\"Person1'\"}"
-- "{\"boss\":null,\"firstName\":\"Karl\",\"lastName\":\"Popper\",\"tag\":\"Person1'\"}"
-- "{\"boss\":\"Jack\",\"firstName\":\"Karl\",\"lastName\":\"Popper\",\"tag\":\"Person2'\"}"
-- "{\"boss\":null,\"firstName\":\"Karl\",\"lastName\":\"Popper\",\"tag\":\"Person2'\"}"

(.->) :: (FromJSON a) => Parser Object -> Key -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

-- { contact_info: { email: <string> } }
nested :: Value -> Parser String
nested = withObject "ContactInfo" $ \obj -> do
  contact <- obj .: "contact_info"
  contact .: "email"

-- λ> parseMaybe nested $ object
--  |   [ "contact_info" .=
--  |     object [ "email" .= "williamyaoh@gmail.com" ]
--  |   ]
-- >>> Just "williamyaoh@gmail.com"
