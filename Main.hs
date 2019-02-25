{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import qualified Data.Aeson as Ae
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as T.L
import GHC.Generics
import qualified Network.Wai.Metrics as Monitoring.HTTP
import Prelude hiding (filter, lookup, not, null, sum, tail)
import qualified System.Remote.Monitoring as Monitoring
import Web.Scotty

not :: Bool -> Bool
not False = True
not True  = False

-- Exercise: Write square

square = undefined

-- Exercise: Write volume

volume = undefined

-- Exercise: Write factorial

factorial = undefined

-------------------------------------------------------------------------------

-- Exercise: Write doubleEven (with guards, with if-then-else)

doubleEven = undefined

-- Exercise: Write weirdMaths

weirdMaths = undefined

-------------------------------------------------------------------------------

-- Exercise: (REPL) (==) and (/=)

-- Exercise: Generalise the types of factorial and volume

-------------------------------------------------------------------------------

-- Exercise: Generalise the type of incrementFst

incrementFst = undefined

-- Exercise: Write fst3, snd3, thd4, fth4

fst3 = undefined
snd3 = undefined
thd4 = undefined
fth4 = undefined

-------------------------------------------------------------------------------

-- Exercise: Write null

null = undefined

-- Exercise: Write tail

tail = undefined

-- Exercise: Write sum

sum = undefined

-- Exercise: Generalise the type of sum

-- Exercise: Write fsts

fsts = undefined

-------------------------------------------------------------------------------

-- Exercise: Write safeHead

safeHead = undefined

-- Exercise: Write maybeLength

maybeLength = undefined

-- Exercise: Write lookup

lookup = undefined

-------------------------------------------------------------------------------

-- Exercise: Write filter

filter = undefined

-- Exercise: Write find

find = undefined

-------------------------------------------------------------------------------

-- Exercise: Write wishHappyBirthday

wishHappyBirthday = undefined

-------------------------------------------------------------------------------

-- Exercise: Write languageMatches

languageMatches = undefined

-- Exercise: Rewrite languageMatches using case

-------------------------------------------------------------------------------

main :: IO ()
main = do
  metricServer <- Monitoring.forkServer "localhost" 7070
  let metricStore = Monitoring.serverMetricStore metricServer
  httpMetrics <- Monitoring.HTTP.registerWaiMetrics metricStore

  db <- newDatabase

  scotty 6060 $ do
    middleware (Monitoring.HTTP.metrics httpMetrics)

    get "/greetings/:name" $ do
      name <- param "name"
      html $ mconcat ["<h1>Hello, ", name, "!</h1>"]

    ---------------------------------------------------------------------------

    -- Feel free to add endpoints here!

    -- Exercise: Add GET /formal-greetings/...

    -- Exercise: Add DELETE /users/...

    -- Exercise: Add GET /languages/favourite

    -- Exercise: Add query-string support to GET /languages

    ---------------------------------------------------------------------------

    get "/languages" $ do
      allLangs <- getLanguages db
      json allLangs

    post "/languages" $ do
      lang <- jsonData
      insertLanguage lang db
      json lang

data Language = Language
  { name        :: Name
  , description :: Description
  }
  deriving (Eq, Ae.FromJSON, Generic, Show, Ae.ToJSON)

type Name        = T.Text
type Description = T.Text

-------------------------------------------------------------------------------

-- Feel free to ignore (in the first instance, and perhaps tinker with in the
-- second) everything below this line.

type Database
  = IORef (M.Map T.Text Language)

newDatabase :: MonadIO m => m Database
newDatabase =
  liftIO $ newIORef mempty

getLanguages :: MonadIO m => Database -> m [Language]
getLanguages db =
  liftIO $ M.elems <$> readIORef db

insertLanguage :: MonadIO m => Language -> Database -> m ()
insertLanguage lang db =
  liftIO $ atomicModifyIORef db (\m -> (M.insert (name lang) lang m, ()))

lookupLanguage :: MonadIO m => Name -> Database -> m (Maybe Language)
lookupLanguage name db =
  liftIO $ M.lookup name <$> readIORef db

getParam :: T.L.Text -> ActionM (Maybe T.Text)
getParam p
  = fmap T.L.toStrict . lookup p <$> params
