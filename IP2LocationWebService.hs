{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
{-|
Module      : IP2LocationWebService
Description : IP2Location Haskell package
Copyright   : (c) IP2Location, 2023 - 2026
License     : MIT
Maintainer  : sales@ip2location.com
Stability   : experimental

This Haskell package allows users to query an IP address to get geolocation info.

IP2Location Web Service API subscription at https://www.ip2location.com/web-service/ip2location
-}
module IP2LocationWebService (WSResult(..), WSConfig, openWS, lookUp, getCredit) where

import Control.Exception
import System.Exit
import Data.Aeson as DA
import Data.Aeson.TH
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.Maybe
import Network.URI.Encode as URIE
import Data.List.Split
import Data.ByteString.Lazy as BS (ByteString, unpack)
import Data.Char (chr)

-- | Contains the web service configuration.
data WSConfig = WSConfig {
    -- | Web service API key
    apiKey :: String,
    -- | API package
    apiPackage :: String,
    -- | Use SSL
    useSSL :: Bool
} deriving (Show)

-- | Contains the web service results.
data WSResult = WSResult {
    -- | Response status or error
    response :: String,
    -- | Country code
    country_code :: Maybe String,
    -- | Country name
    country_name :: Maybe String,
    -- | Region name
    region_name :: Maybe String,
    -- | City name
    city_name :: Maybe String,
    -- | Latitude
    latitude :: Maybe Float,
    -- | Longitude
    longitude :: Maybe Float,
    -- | ZIP code
    zip_code :: Maybe String,
    -- | Time zone
    time_zone :: Maybe String,
    -- | ISP name
    isp :: Maybe String,
    -- | Domain
    domain :: Maybe String,
    -- | Net speed
    net_speed :: Maybe String,
    -- | IDD code
    idd_code :: Maybe String,
    -- | Area code
    area_code :: Maybe String,
    -- | Weather station code
    weather_station_code :: Maybe String,
    -- | Weather station name
    weather_station_name :: Maybe String,
    -- | MCC
    mcc :: Maybe String,
    -- | MNC
    mnc :: Maybe String,
    -- | Mobile brand
    mobile_brand :: Maybe String,
    -- | Elevation
    elevation :: Maybe Float,
    -- | Usage type
    usage_type :: Maybe String,
    -- | Address type
    address_type :: Maybe String,
    -- | IAB category code
    category :: Maybe String,
    -- | IAB category name
    category_name :: Maybe String,
    -- | Credits consumed
    credits_consumed :: Maybe Float
} deriving (Show, Eq)

$(deriveJSON defaultOptions ''WSResult)

checkparams :: String -> String -> IO String
checkparams apikey apipackage = do
    return "OK"
    --- regex part commented out due to cabal dependency issues
    -- let apikeyok = apikey =~ ("^[0-9A-Z]{10}$" :: String) :: Bool
    -- if apikeyok == False
        -- then die(show "Invalid API key.")
        -- else do
            -- let apipackageok = apipackage =~ ("^WS[0-9]+$" :: String) :: Bool
            -- if apipackageok == False
                -- then die(show "Invalid package name.")
                -- else return "OK"

{-|
    The 'openWS' function initializes the web service configuration.
    It takes 3 arguments; the web service API key, the API package to call & whether to use SSL.
-}
openWS :: String -> String -> Bool -> IO WSConfig
openWS apikey apipackage usessl = do
    paramok <- checkparams apikey apipackage
    return (WSConfig apikey apipackage usessl)

{-|
    The 'lookUp' function returns an WSResult containing geolocation data for an IP address
    It takes 2 arguments; the web service configuration from 'openWS' function (WSConfig record), either IPv4 or IPv6 address (String)
-}
lookUp :: WSConfig -> String -> IO WSResult
lookUp myconfig ip = do
    let key = apiKey myconfig
    let package = apiPackage myconfig
    let usessl = useSSL myconfig

    paramok <- checkparams key package
    let protocol = if usessl == True
        then "https"
        else "http"
    manager <- newManager tlsManagerSettings
    httprequest <- parseRequest $ protocol ++ "://api.ip2location.com/v2/?key=" ++ key ++ "&package=" ++ package ++ "&ip=" ++ (URIE.encode ip)
    httpresponse <- httpLbs httprequest manager
    let json = responseBody httpresponse
    let Just result = DA.decode json :: Maybe WSResult
    return result

bsToString :: BS.ByteString -> String
bsToString bs = map (chr . fromEnum) . BS.unpack $ bs

{-|
    The 'getCredit' function returns an IO String containing web service credit balance for the API key.
    It takes 1 argument; the web service configuration from 'openWS' function (WSConfig record).
-}
getCredit :: WSConfig -> IO String
getCredit myconfig = do
    let key = apiKey myconfig
    let package = apiPackage myconfig
    let usessl = useSSL myconfig

    paramok <- checkparams key package
    let protocol = if usessl == True
        then "https"
        else "http"
    manager <- newManager tlsManagerSettings
    httprequest <- parseRequest $ protocol ++ "://api.ip2location.com/v2/?key=" ++ key ++ "&check=true"
    httpresponse <- httpLbs httprequest manager
    let json = responseBody httpresponse
    -- using splitOn to extract the response field to bypass the Haskell duplicate field name issues
    let part = head (splitOn "}" (bsToString json))
    let result = last (splitOn ":" part)
    return result
