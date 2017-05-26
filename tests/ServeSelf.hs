module Main where

import Network.Wai.Handler.Warp
import Web.Pager
import Data.Default.Class

main = run 8080 $ pagerApp "src/Web/Pager.hs" def

