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
import Prelude hiding (filter, lookup, not, null, tail)
import Web.Scotty

--  Exercise: fire up the REPL, play with some basic bits:
--    Arithmetic: 2, 2 + 5, 3 * 4, 4 * (5 + 3)
--      Precedence, etc.
--    Booleans: True, False, not True
--      Predicates and logical operators: even 2, 3 > 1, odd 5 && even 2
--      Talk about Eq and Ord
--      Even and odd are functions -- talk about juxtaposition as application
--    Characters and strings: 'c', "hello"
--      Unlike JS, quotes are important
--    Tuples: ('c', False), (False, 1, True)
--      fst, snd
--      Don't confuse passing a tuple with calling an n-argument function
--    Lists: [], [1,2,3], [1..10]
--      Superficially similar to JavaScript's arrays, but not the same
--      Functions that work on lists: length
--
--    Come out comfortable with function application, operators, basic types
--  Exercise: write a quadratic evaluator
--    Int -> Int -> Int
--    Can we do Float -> Float -> Float
--    Can we do any number? Num a => a -> a -> a
--  Exercise: write factorial
--    Come out comfortable with recursion
--  Exercise: write null, tail
--    Come out comfortable with pattern matching on lists
--  Exercise: write fst3, snd3, thd4, fth4
--    Come out comfortable with pattern matching on tuples
--  Exercise: write filter
--    Let's just get the even numbers out
--  Exercise: write lookup
--    Now you only want _one_, but what if it's not there -- introduce Maybe,
--    Just, Nothing
--  Exercise: write languageMatches
--  Exercise: introduce filter into GET /languages

not :: Bool -> Bool
not False = True
not True  = False

quadratic :: Int -> Int -> Int -> Int -> Int
quadratic a b c x =
  a * x * x + b * x + c

factorial :: Int -> Int
factorial 0 =
  1
factorial n =
  n * factorial (n - 1)

null :: [a] -> Bool
null [] =
  True
null (x : xs) =
  False

tail :: [a] -> [a]
tail (x : xs) =
  xs
tail [] =
  []

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

thd4 :: (a, b, c, d) -> c
thd4 (a, b, c, d) = c

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

filter :: (a -> Bool) -> [a] -> [a]
filter p [] =
  []
filter p (x : xs) =
  if p x then x : filter p xs else filter p xs

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup k [] =
  Nothing
lookup k ((k', v) : ps)
  | k == k'   = Just v
  | otherwise = lookup k ps

main :: IO ()
main = do
  db <- newDatabase

  scotty 6060 $ do
    get "/greetings/:name" $ do
      name <- param "name"
      html $ mconcat ["<h1>Hello, ", name, "!</h1>"]

    get "/languages" $ do
      allLangs <- getLanguages db
      maybePrefix <- getParam "prefix"
      let preprocess = case maybePrefix of
            Nothing ->
              id
            Just prefix ->
              filter (languageMatches prefix)
      json (preprocess allLangs)

    post "/languages" $ do
      lang <- jsonData
      insertLanguage lang db
      json lang

languageMatches :: T.Text -> Language -> Bool
languageMatches prefix lang =
  prefix `T.isPrefixOf` name lang

data Language = Language
  { name        :: Name
  , description :: Description
  }
  deriving (Eq, Ae.FromJSON, Generic, Show, Ae.ToJSON)

type Name        = T.Text
type Description = T.Text
type Database    = IORef (M.Map T.Text Language)

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
