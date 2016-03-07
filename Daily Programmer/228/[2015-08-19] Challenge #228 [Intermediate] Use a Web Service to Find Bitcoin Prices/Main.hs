module Main where

import Network.HTTP

import Control.Monad.Trans.Maybe
import Control.Monad.Trans (liftIO)
import Control.Monad (forever, guard, msum, mzero)

import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.List (lookup, sort, intercalate, (!!))
import Data.List.Split (splitOn, chunksOf)

type URL      = String
type Market   = String
type Currency = String
type Price    = Float

liftMaybe :: Maybe a -> MaybeT IO a
liftMaybe = MaybeT . return

matchError :: String -> Maybe b -> MaybeT IO b
matchError errorMessage =
    maybe (liftIO (putStrLn ('\n' : errorMessage) >> putStr "Please try again: ") >> mzero) return

tryUntilSucessfull :: MaybeT IO a -> IO a
tryUntilSucessfull = fmap fromJust . runMaybeT . msum . repeat
    
getCurrentBitcoinPrice :: MaybeT IO Price
getCurrentBitcoinPrice = do
    let baseURL = "http://api.bitcoincharts.com/v1/trades.csv?symbol="
        markets = sort ["bitfinex", "bitstamp", "btce", "itbit", "anxhk", "hitbtc"
                       ,"kraken", "bitkonan", "bitbay", "rock", "cbx", "cotr", "vcx"]
        currencies = sort ["KRW", "NMC", "IDR", "RON", "ARS", "AUD", "BGN", "BRL", "BTC"
                          , "CAD", "CHF", "CLP", "CNY", "CZK", "DKK", "EUR", "GAU"
                          ,"GBP", "HKD", "HUF", "ILS", "INR", "JPY", "LTC", "MXN", "NOK"
                          , "NZD", "PEN", "PLN", "RUB", "SAR", "SEK", "SGD", "SLL"
                          , "THB", "UAH", "USD", "XRP", "ZAR"]
    market   <- liftIO $ giveChoice "\nSelect a market:"   markets
    currency <- liftIO $ giveChoice "Select a currency:" currencies
    responseBody <- liftIO $ getResponseBody =<< simpleHTTP (getRequest $ baseURL ++ market ++ currency)
    guard $ not (null responseBody)
    liftMaybe . readMaybe $ splitOn "," responseBody !! 1

giveChoice :: String -> [String] -> IO String
giveChoice prompt choices = do
    let annotatedChoices = zip [1..] choices
        display = unlines . map (intercalate "\t") . chunksOf 3 . map displayChoice
        displayChoice (n, c) = "  (" ++ show n ++ ") " ++ c
    putStrLn prompt
    putStr $ display annotatedChoices
    return =<< tryUntilSucessfull $ do
        input <- liftIO getLine
        choiceNumber <- matchError "Invalid Input: Input must be an integer." (readMaybe input)
        matchError "Invalid Input: Out-of-range." (lookup choiceNumber annotatedChoices)
          
main :: IO ()
main = forever $ do 
  price <- runMaybeT getCurrentBitcoinPrice
  putStrLn $ maybe "\nThis API is flaky. Please try again." (("\nPrice: " ++) . show) price