module UntappdSpec where

import           Control.Exception         (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad.Trans.Maybe

import           Untappd
import    qualified       Data.Aeson as A

-- see example tip for better errors at https://github.com/hspec/hspec/issues/334
-- @example@ is a type restricted version of `id`.  It can be used to get better
-- error messages on type mismatches.
spec :: Spec
spec = do
  describe "Untappd" $ do

    it "beerInfo should return beer info" $ example $ do
      pendingWith "disabled because of api rate limits"
      r <- beerInfo 2018
      _ <- putStrLn $ show r
      r `shouldBe`
        (Right $
         Beer
           2018
           "Petrus Aged Pale"
           "Petrus Aged Pale is a 100% uncut beer from our oak foeders and aged for 24 months. Michael Jackson, the beer hunter, chose the name in the late 1990's and he was the first to sell it in his beer clubs. Today, it is still the reference for SOUR beers in the US. Petrus Aged Pale has won multiple gold medals in beer competitions worldwide. Moreover, ever since the start of the brewery, Petrus Aged Pale is used as a blending beer to give other beers fresh and slightly sour flavours and aromas. Cheers!")

    it "breweryInfo should return brewery info" $ example $ do
      pendingWith "disabled because of api rate limits"
      info <- breweryInfo 4571
      _ <- putStrLn $ show info
      let info' = fmap withoutCheckins info
      info' `shouldBe` (Right $ Brewery 4571 "Brouwerij De Brabandere" ( Just $ CheckinsWrapper 0 []))

    it "breweryFeed should return checkins" $ example $ do
      pendingWith "disabled because of api rate limits"
      r <- breweryFeed 4571
      _ <- putStrLn $ show r
      r `shouldBe` Right []

-- TODO lenses might help to simplify this
withoutCheckins :: Brewery -> Brewery
withoutCheckins b =
  Brewery
    (brewery_id b)
    (brewery_name b)
    (Just $ CheckinsWrapper 0 [])
