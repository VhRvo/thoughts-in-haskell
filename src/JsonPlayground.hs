

module JsonPlayground where

import Data.Aeson
import Data.Aeson.Types

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