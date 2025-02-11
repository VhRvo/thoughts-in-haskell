module Bifunctor.Demo where

import Bifunctor.Class hiding (step)
import Data.Foldable qualified as DF
import Data.Text (Text)
import Prelude hiding (map)

type Doc = Fix DocF Text

data DocF a b = Para a | Section Text [b]

instance Bifunctor DocF where
  bimap f g = \case
    Para s -> Para (f s)
    Section s xs -> Section s (g <$> xs)

correct :: Text -> Text
correct = undefined

corrector :: Doc -> Doc
corrector = map correct

combine :: DocF Text [Text] -> [Text]
combine = \case
  Para s -> [s]
  Section s xs -> s : DF.fold xs

printDoc :: Doc -> [Text]
printDoc = fold combine

data XML
  = Text Text
  | Entity Tag Attributes [XML]

type Tag = Text

type Attributes = [(Text, Text)]

fromXML :: XML -> Doc
fromXML = unfold step

step :: XML -> DocF Text XML
step = \case
  Text text -> Para text
  Entity tag attributes xs -> Section (title tag attributes) xs

title :: Tag -> Attributes -> Text
title tag = \case
  [] -> tag
  kv : kvs -> tag <> paren (join (attr kv) (attr <$> kvs))
  where
    join tag [] = tag
    join tag (s : ss) = tag <> ", " <> join s ss
    attr (key, value) = key <> "='" <> value <> "'"
    paren s = " (" <> s <> ")"

printXML :: XML -> [Text]
printXML = hylo step combine

buildDoc :: (DocF Text b -> b) -> b
buildDoc f = f (Section "Heading" [f (Para "p1"), f (Para "p2")])

myDoc :: Doc
myDoc = build buildDoc

printMyDoc :: [Text]
printMyDoc = buildDoc combine
