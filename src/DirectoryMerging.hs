module DirectoryMerging where

import Data.Aeson
import Data.Aeson qualified as J
import Data.Text (Text)

merge :: [([Text], Text, J.Object)] -> J.Object
merge objects = undefined -- objects
