{-# LANGUAGE OverloadedStrings #-}

module Pings (
  query
) where

import qualified Codec.Binary.UTF8.String      as U
import           Data.Aeson
import qualified Data.ByteString.Lazy          as B
import qualified Data.JSString                 as J
import JavaScript.Web.XMLHttpRequest

query :: String -> IO (Maybe String)
query bearer = do
    contents <$> xhrString req
    where
        req = Request
            { reqMethod = POST
            , reqURI = "http://172.20.0.2/graphiql?"
            , reqLogin = Nothing
            , reqHeaders = [("authorization", "bearer " <> (J.pack bearer))]
            , reqWithCredentials = False
            , reqData = StringData $ J.pack  $ "query {\n  pings {\n    id\n    insertedAt\n    updatedAt\n    source\n    destination\n    ttl\n    time\n    history {\n    \tttl\n      source\n      time\n      destination\n    }\n  }\n}"
            }
