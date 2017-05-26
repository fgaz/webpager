{-# LANGUAGE OverloadedStrings #-}
module Web.Pager where

import Data.Default.Class
import Lucid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Text.Encoding as TE
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import Network.Wai
import Network.HTTP.Types.URI (Query)
import Network.HTTP.Types.Status (status200)
import Data.Semigroup ((<>))
import Data.Monoid (mempty)
import Control.Monad (when)
import Data.Maybe (fromMaybe, catMaybes)

--TODO rename to Serrings/Config?
data Metadata = Metadata { title :: Text
                         , extraMetas :: Html () 
                         , perPage :: Int }

instance Default Metadata where
  def = Metadata { title = "Pager"
                 , extraMetas = mempty
                 , perPage = 20 }

pagerMiddleware :: FilePath -> Metadata -> Middleware
pagerMiddleware path meta =
  ifRequest isPagerRequest $ const $ pagerApp path meta
  where isPagerRequest = const True --TODO

pagerApp :: FilePath -> Metadata -> Application
pagerApp path meta req respond = do
  let n = requestToPageNumber req
  responseBody <- handler path meta n
  respond $ responseLBS status200 [] responseBody

requestToPageNumber :: Request -> Maybe Int
requestToPageNumber = (readMaybeBSDecimal=<<)
                    . fmap snd
                    . safeHead
                    . filter ((=="p") . fst)
                    . catMaybes
                    . fmap sequenceA
                    . queryString

readMaybeBSDecimal :: SBS.ByteString -> Maybe Int
readMaybeBSDecimal lbs =
  case TE.decodeUtf8' lbs of Left _ -> Nothing
                             Right txt -> readMaybeTDecimal txt

readMaybeTDecimal :: Text -> Maybe Int
readMaybeTDecimal txt =
  case TR.decimal txt of Left _ -> Nothing
                         Right (n,_) -> Just n

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

handler :: FilePath -> Metadata -> Maybe Int -> IO ByteString
handler path meta n' = do
  contents <- TIO.readFile path
  let ls = T.lines contents
  let nlines = length ls
  let lastPage = nlines `div` perPage meta
  let n = fromMaybe lastPage n'
  return $ renderBS $ wrapHtml meta n lastPage $ page meta ls n

page :: Metadata -> [Text] -> Int -> Html ()
page meta ls' n =
  table_ $ do
    th_ $ td_  "This is the table head. TODO"
    mapM_ toTableRow ls
  where ls = take (perPage meta)
           $ drop (perPage meta * n) ls'

--TODO manage splitting with user given options
--TODO ESCAPE
toTableRow :: Text -> Html ()
toTableRow = tr_ . td_  . toHtml

pageSelector :: Int -> Int -> Html ()
pageSelector n lastPage =
  div_ [class_ "pageSelector"] $ do
    when (n /= 0) $ do
      span_ [class_ "firstPage"] $ a_ [href_ "?p=0"] "First"
      span_ [class_ "prevPage"] $ a_ [href_ $ ("?p=" <>) $ T.pack $ show (n-1)] "Previous"
    when (n /= lastPage) $ do
      span_ [class_ "nextPage"] $ a_ [href_ $ ("?p=" <>) $ T.pack $ show (n+1)] "Next"
      span_ [class_ "lastPage"] $ a_ [href_ $ ("?p=" <>) $ T.pack $ show lastPage] "Last"

wrapHtml :: Metadata -> Int -> Int -> Html () -> Html ()
wrapHtml meta n lastPage inner = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ $ toHtml $ title meta
      --metas
      --assets
      extraMetas meta
    body_ $ do
      header
      h2_ ("Page " <> toHtml (T.pack $ show n))
      div_ [class_ "wrap"] $ main_ [role_ "main"] $ do
        pageSelector n lastPage
        div_ [class_ "content"] inner
        pageSelector n lastPage
      hr_ []
      footer

header :: Html ()
header = header_ [role_ "banner"] $
  h1_ "Pager"

footer :: Html ()
footer = footer_ [role_ "contentinfo"] $
  p_ $ do
    "Powered by "
    a_ [href_ "https://github.com/fgaz/webpager"] "webpager"

