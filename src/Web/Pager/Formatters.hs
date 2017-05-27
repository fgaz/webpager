{-# LANGUAGE OverloadedStrings #-}

module Web.Pager.Formatters
( colorHash
, irc
, oneWordPerCell
) where

import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Data.Semigroup ((<>))
import Numeric (showHex)
import Data.Hashable (hash)

type Formatter = Text -> [Html ()]

colorHash :: Text -> Attribute
colorHash t = style_ ("color:#" <> T.take 6 (hash' t) <>";")

hash' :: Text -> Text
hash' = T.pack . flip showHex "" . abs . hash

irc :: Formatter
irc txt = [dateHtml, hourHtml, nameHtml, messageHtml]
  where dateHtml = toHtml $ T.takeWhile (/=' ') txt
        txt' = T.drop 1 $ T.dropWhile (/=' ') txt
        hourHtml = toHtml $ T.takeWhile (/=' ') txt'
        txt'' = T.drop 1 $ T.dropWhile (/=' ') txt'
        name = T.takeWhile (/=' ') txt''
        message = T.drop 1 $ T.dropWhile (/=' ') txt''
        nameColor = colorHash name
        nameHtml = span_ [nameColor] $ toHtml name
        messageHtml = toHtml message

oneWordPerCell :: Formatter
oneWordPerCell txt = toHtml <$> T.words txt

