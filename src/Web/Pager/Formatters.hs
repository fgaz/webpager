{-# LANGUAGE OverloadedStrings #-}

module Web.Pager.Formatters
( Formatter
, colorHash
, irc
, ircUnixTime
, oneWordPerCell
) where

import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import Data.Hashable (hash)
import Data.Time

-- | A formatter turns a line into a list of HTML fragments.
-- Each fragment will be wrapped in a <td> (you don't need
-- to do it (and in fact you shouldn't)) and put into a <tr>
-- together with the rest of the line.
type Formatter = Text -> [Html ()]

-- | Utility function, used internally in 'irc'.
-- Creates a color attribute by hashing the contents of its
-- argument.
-- Useful for usernames and such.
colorHash :: Text -> Attribute
colorHash t = style_ ("color:#" <> T.take 6 (hash' t) <>";")

hash' :: Text -> Text
hash' = T.pack . flip showHex "" . abs . hash

-- | Parse irc logs, in the format outputted by the ii client.
--
-- > "date time <user> message (maybe with spaces)
--
-- becomes
-- [date, time, uniquely colored <user>, message (maybe with spaces)]
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

-- | Parse irc logs, in _another_ format outputted by the ii client.
--
-- > "unixtime <user> message (maybe with spaces)
--
-- becomes
-- [date, time, uniquely colored <user>, message (maybe with spaces)]
ircUnixTime :: Formatter
ircUnixTime txt = [dateHtml, hourHtml, nameHtml, messageHtml]
  where timestamp = T.takeWhile (/=' ') txt
        utcTime = fromMaybe zeroTime $
                    parseTimeM
                      True
                      defaultTimeLocale
                      "%s"
                      $ T.unpack timestamp
        localTime = utcToLocalTime utc utcTime
        dateHtml = toHtml $ showGregorian $ localDay localTime
        hourHtml = toHtml $ show $ localTimeOfDay localTime
        txt' = T.drop 1 $ T.dropWhile (/=' ') txt
        name = T.takeWhile (/=' ') txt'
        message = T.drop 1 $ T.dropWhile (/=' ') txt'
        nameColor = colorHash name
        nameHtml = span_ [nameColor] $ toHtml name
        messageHtml = toHtml message
        zeroTime = UTCTime (ModifiedJulianDay 0) 0

-- | Separates the words in the line, one cell each.
oneWordPerCell :: Formatter
oneWordPerCell txt = toHtml <$> T.words txt

