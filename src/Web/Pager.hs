{-# LANGUAGE OverloadedStrings #-}

module Web.Pager
( Config(..)
, pagerMiddleware
, pagerApp
) where

import Data.Default.Class
import Lucid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import qualified Data.Text.Encoding as TE
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as SBS
import Network.Wai
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Types.Header (hContentType)
import Data.Semigroup ((<>))
import Data.Monoid (mempty)
import Control.Monad (when)
import Data.Maybe (fromMaybe, catMaybes)

data Config = Config { title :: Text
                     , extraMetas :: Html () 
                     , perPage :: Int
                     , tableHead :: [Text]
                     , formatLine :: Text -> [Html ()]}

instance Default Config where
  def = Config { title = "Pager"
               , extraMetas = mempty
               , perPage = 20
               , tableHead = ["Lines"]
               , formatLine = pure . toHtml }

pagerMiddleware :: (Request -> Bool) -> FilePath -> Config -> Middleware
pagerMiddleware isPagerRequest path conf =
  ifRequest isPagerRequest $ const $ pagerApp path conf

pagerMiddlewareOnPath :: Text {-TODO or whatever string-like is fit-} -> FilePath -> Config -> Middleware
pagerMiddlewareOnPath = pagerMiddleware . mkCondition
  where mkCondition = undefined --TODO

pagerApp :: FilePath -> Config -> Application
pagerApp path conf req respond = do
  let n = requestToPageNumber req
  responseBody <- handler path conf n
  respond $ responseLBS status200 [(hContentType, "text/html")] responseBody

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

handler :: FilePath -> Config -> Maybe Int -> IO ByteString
handler path conf n' = do
  contents <- TIO.readFile path
  let ls = T.lines contents
  let nlines = length ls
  let lastPage = nlines `div` perPage conf
  let n = fromMaybe lastPage n'
  return $ renderBS $ wrapHtml conf n lastPage $ page conf ls n

page :: Config -> [Text] -> Int -> Html ()
page conf ls' n =
  table_ $ do
    tr_ $
      mapM_ (th_ . toHtml) $ tableHead conf
    mapM_ (toTableRow $ formatLine conf) ls
  where ls = take (perPage conf)
           $ drop (perPage conf * n) ls'

toTableRow :: (Text -> [Html ()]) -> Text -> Html ()
toTableRow format = tr_ . mapM_ td_  . format

pageSelector :: Int -> Int -> Html ()
pageSelector n lastPage =
  div_ [class_ "pageSelector"] $ do
    when (n /= 0) $ do
      span_ [class_ "firstPage"] $ a_ [href_ "?p=0"] "First"
      span_ [class_ "prevPage"] $ a_ [href_ $ ("?p=" <>) $ T.pack $ show (n-1)] "Previous"
    when (n /= lastPage) $ do
      span_ [class_ "nextPage"] $ a_ [href_ $ ("?p=" <>) $ T.pack $ show (n+1)] "Next"
      span_ [class_ "lastPage"] $ a_ [href_ $ ("?p=" <>) $ T.pack $ show lastPage] "Last"

wrapHtml :: Config -> Int -> Int -> Html () -> Html ()
wrapHtml conf n lastPage inner = do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      title_ $ toHtml $ title conf
      --metas
      --assets
      extraMetas conf
    body_ $ do
      header $ title conf
      h2_ ("Page " <> toHtml (T.pack $ show n))
      div_ [class_ "wrap"] $ main_ [role_ "main"] $ do
        pageSelector n lastPage
        div_ [class_ "content"] inner
        pageSelector n lastPage
      hr_ []
      footer

header :: Text -> Html ()
header t = header_ [role_ "banner"] $
  h1_ $ toHtml t

footer :: Html ()
footer = footer_ [role_ "contentinfo"] $
  p_ $ do
    "Powered by "
    a_ [href_ "https://github.com/fgaz/webpager"] "webpager"

