{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Untappd
Description : Untappd api client

Untappd api docs are available at https://untappd.com/api/docs

Make sure you have UNTAPPD_CLIENT_ID and UNTAPPD_CLIENT_SECRET env vars set
-}
module Untappd where

import           Control.Monad              (mzero)
import           Control.Monad.Trans.Maybe  (MaybeT)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Either
import           Data.Text                  (Text)
import qualified Data.Yaml                  as Yaml
import           GHC.Generics               (Generic)
import           Network.HTTP.Simple
import           System.Envy

data UnappdConfig = UnappdConfig
  { untappdClientId     :: String
  , untappdClientSecret :: String
  } deriving (Generic, Show)

instance FromEnv UnappdConfig where
  fromEnv =
    UnappdConfig <$> env "UNTAPPD_CLIENT_ID"
                 <*> env "UNTAPPD_CLIENT_SECRET"

authParams :: UnappdConfig -> String
authParams uc = "client_id=" ++ untappdClientId uc ++ "&client_secret=" ++ untappdClientSecret uc

base = "https://api.untappd.com/v4"

data GenericResponse = GenericResponse
  { response :: Value
  } deriving (Show, Generic, Eq)

data BeerResponse = BeerReponse
  { beer :: Beer
  } deriving (Show, Generic, Eq)


data BreweryResponse = BreweryResponse
  { brewery :: Brewery
  } deriving (Show, Generic, Eq)

data BreweryCheckins = BreweryCheckins
  { --pagination :: Value
   checkins :: CheckinsWrapper
  } deriving (Show, Generic, Eq)

data User = User
  { uid        :: Int
  , user_name  :: !Text
  , first_name :: !Text
  } deriving (Show, Generic, Eq)

data Beer = Beer
  { bid              :: Int
  , beer_name        :: !Text
  , beer_description :: !Text
  } deriving (Show, Generic, Eq)

data Brewery = Brewery
  { brewery_id   :: Int
  , brewery_name :: !Text
  , checkins     :: Maybe CheckinsWrapper -- might be missing in case of a checking nested brewery
  } deriving (Show, Generic, Eq)

data Toast = Toast
  {
    foo :: Maybe Int
  } deriving (Show, Generic, Eq)

data Toasts = Toasts
  { items       :: [Toast]
  , total_count :: Int
  , auth_toast  :: Maybe Bool
  } deriving (Show, Generic, Eq)

data Checkin = Checkin
  { checkin_id      :: Int
  , rating_score    :: Double
  , checkin_comment :: !Text
  , beer            :: Beer
  , user            :: User
  , brewery         :: Brewery
  , toasts          :: Toasts
  } deriving (Show, Generic, Eq)

data CheckinsWrapper = CheckinsWrapper
  { count :: Int
  , items :: [Checkin]
  } deriving (Show, Generic, Eq)

--instance FromJSON Person where
-- parseJSON (Object v) =
--    Person <$> v .: "firstName"
--           <*> v .: "lastName"
--           <*> v .: "age"
--           <*> v .: "likesPizza"
-- parseJSON _ = mzero
--
--instance ToJSON Person where
-- toJSON (Person firstName lastName age likesPizza) =
--    object [ "firstName"  .= firstName
--           , "lastName"   .= lastName
--           , "age"        .= age
--           , "likesPizza" .= likesPizza
--             ]
instance FromJSON User

instance ToJSON User

instance FromJSON Beer

instance ToJSON Beer

instance FromJSON Brewery

instance ToJSON Brewery

instance FromJSON Toast

instance ToJSON Toast

instance FromJSON Toasts

instance ToJSON Toasts

instance FromJSON Checkin

instance ToJSON Checkin

instance FromJSON CheckinsWrapper

instance ToJSON CheckinsWrapper

instance FromJSON BeerResponse

instance FromJSON BreweryResponse

instance FromJSON GenericResponse

instance ToJSON GenericResponse


instance ToJSON BreweryCheckins

instance FromJSON BreweryCheckins

config :: IO UnappdConfig
config = do
  env <- decodeEnv :: IO (Either String UnappdConfig)
  pure $ case env of
    Left err -> error err
    Right c -> c

jsonReq :: String -> IO Value
jsonReq path = do
  c <- config
  let fullUrl = base ++ path ++ "?" ++ authParams c
  req <- parseRequest fullUrl
  resp <- httpJSON req
  putStrLn $ "The status code was: " ++ show (getResponseStatusCode resp)
  print $ head $ getResponseHeader "Content-Type" resp
  putStrLn $ "X-Ratelimit-Limit: " ++ show (head $ getResponseHeader "X-Ratelimit-Limit" resp)
  putStrLn $ "X-Ratelimit-Remaining: " ++ show (head $ getResponseHeader "X-Ratelimit-Remaining" resp)
  let body = getResponseBody resp :: Value
  LB.putStrLn $ LB.take 1000 $ encodePretty body
  pure body

fromJsonReq :: (FromJSON a) => String -> IO (Either String a)
fromJsonReq path = do
  body <- jsonReq path
  let resp = (fromJSON :: Value -> Result GenericResponse) body
  let resp' = fmap response resp
  let resp'' = resp' >>= fromJSON
  pure $ resultToEither resp''

breweryInfo :: Int -> IO (Either String Brewery)
breweryInfo breweryId = do
  body <- (fromJsonReq :: String -> IO (Either String BreweryResponse)) $ "/brewery/info/" ++ show breweryId
  pure $ fmap (brewery :: BreweryResponse -> Brewery) body

breweryFeed :: Int -> IO (Either String [Checkin])
breweryFeed breweryId = do
  body <- fromJsonReq $ "/brewery/checkins/" ++ show breweryId
  pure $ fmap respCheckins body

beerInfo :: Int -> IO (Either String Beer)
beerInfo beerId = do
  body <- fromJsonReq $ "/beer/info/" ++ show beerId
  pure $ fmap (beer :: BeerResponse -> Beer) body

respCheckins :: BreweryCheckins -> [Checkin]
respCheckins =
  (items :: CheckinsWrapper -> [Checkin]) .
  (checkins :: BreweryCheckins -> CheckinsWrapper)

resultToEither :: Result a -> Either String a
resultToEither (Error msg) = Left msg
resultToEither (Success a) = Right a

cli :: IO ()
cli =
  -- TODO add cli
  putStrLn "CLI not implemented yet"
