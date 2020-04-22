{-|
Module      : IP2Location
Description : IP2Location Haskell package
Copyright   : (c) IP2Location, 2020
License     : MIT
Maintainer  : sales@ip2location.com
Stability   : experimental

This Haskell package provides a fast lookup of country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type,
IDD code, area code, weather station code, weather station name, mcc, mnc, mobile brand, elevation, and usage type from IP address by using IP2Location database.
This package uses a file based database available at IP2Location.com. This database simply contains IP blocks as keys, and other information such as country, 
region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, weather station name, mcc, mnc, 
mobile brand, elevation, and usage type as values. It supports both IP addresses in IPv4 and IPv6.

IP2Location LITE BIN databases are available for free at http://lite.ip2location.com/
-}
module IP2Location (Meta, IP2LocationRecord(..), getAPIVersion, doInit, doQuery) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Word
import Data.Bits
import Data.Binary.Get
import Data.IP
import Control.Exception

-- | Contains geolocation results.
data IP2LocationRecord = IP2LocationRecord {
    -- | Country code
    country_short :: String,
    -- | Country name
    country_long :: String,
    -- | Region name
    region :: String,
    -- | City name
    city :: String,
    -- | ISP name
    isp :: String,
    -- | Latitude
    latitude :: Float,
    -- | Longitude
    longitude :: Float,
    -- | Domain name
    domain :: String,
    -- | ZIP/Postal code
    zipcode :: String,
    -- | Timezone
    timezone :: String,
    -- | Network speed
    netspeed :: String,
    -- | IDD code
    iddcode :: String,
    -- | Area code
    areacode :: String,
    -- | Weather station code
    weatherstationcode :: String,
    -- | Weather station name
    weatherstationname :: String,
    -- | Mobile country code
    mcc :: String,
    -- | Mobile network code
    mnc :: String,
    -- | Carrier brand
    mobilebrand :: String,
    -- | Elevation in meters
    elevation :: Float,
    -- | Usage type
    usagetype :: String
} deriving (Show)

-- | Contains the BIN database file metadata.
data Meta = Meta {
    -- | Database type
    databasetype :: Int,
    -- | Number of columns
    databasecolumn :: Int,
    -- | Database year
    databaseyear :: Int,
    -- | Database month
    databasemonth :: Int,
    -- | Database day
    databaseday :: Int,
    -- | IPv4 data count
    ipv4databasecount :: Int,
    -- | IPv4 data base address
    ipv4databaseaddr :: Int,
    -- | IPv6 data count
    ipv6databasecount :: Int,
    -- | IPv6 data base address
    ipv6databaseaddr :: Int,
    -- | IPv4 index base address
    ipv4indexbaseaddr :: Int,
    -- | IPv6 index base address
    ipv6indexbaseaddr :: Int,
    -- | IPv4 column size
    ipv4columnsize :: Int,
    -- | IPv6 column size
    ipv6columnsize :: Int
} deriving (Show)

getMeta = do
    databasetype <- getWord8
    databasecolumn <- getWord8
    databaseyear <- getWord8
    databasemonth <- getWord8
    databaseday <- getWord8
    ipv4databasecount <- getWord32le
    ipv4databaseaddr <- getWord32le
    ipv6databasecount <- getWord32le
    ipv6databaseaddr <- getWord32le
    ipv4indexbaseaddr <- getWord32le
    ipv6indexbaseaddr <- getWord32le
    let ipv4columnsize = fromIntegral databasecolumn `shiftL` 2 -- 4 bytes each column
    let ipv6columnsize = 16 + ((fromIntegral databasecolumn - 1) `shiftL` 2) -- 4 bytes each column, except IPFrom column which is 16 bytes
    let meta = Meta (fromIntegral databasetype) (fromIntegral databasecolumn) (fromIntegral databaseyear) (fromIntegral databasemonth) (fromIntegral databaseday) (fromIntegral ipv4databasecount) (fromIntegral ipv4databaseaddr) (fromIntegral ipv6databasecount) (fromIntegral ipv6databaseaddr) (fromIntegral ipv4indexbaseaddr) (fromIntegral ipv6indexbaseaddr) ipv4columnsize ipv6columnsize
    return meta

{-|
    The 'getAPIVersion' function returns a string containing the API version.
-}
getAPIVersion :: String
getAPIVersion = "8.2.1"

ipToOcts :: IP -> [Int]
ipToOcts (IPv4 ip) = fromIPv4 ip
ipToOcts (IPv6 ip) = fromIPv6b ip

ipToInteger :: IP -> Integer
ipToInteger = sum . map (\(n,o) -> toInteger o * 256 ^ n) . zip [0..] . reverse . ipToOcts

ipStringToInteger :: String -> Integer
ipStringToInteger = ipToInteger . read

{-|
    The 'doInit' function returns the Meta record containing metadata from the BIN database file.
    It takes one argument, of type 'String', which is the path to the BIN database file.
-}
doInit :: String -> IO Meta
doInit myfile = do
    contents <- BS.readFile myfile
    return $ runGet getMeta contents

readuint8 :: BS.ByteString -> Int -> Int
readuint8 contents startpos = fromIntegral (runGet getWord8 (BS.drop (fromIntegral startpos - 1) contents))

readuint32 :: BS.ByteString -> Int -> Int
readuint32 contents startpos = fromIntegral (runGet getWord32le (BS.drop (fromIntegral startpos - 1) contents))

readuint32row :: BS.ByteString -> Int -> Int
readuint32row row startpos = fromIntegral (runGet getWord32le (BS.drop (fromIntegral startpos) row))

getuint128 = do
    uint64A <- getWord64le
    uint64B <- getWord64le
    let uint128 = (toInteger uint64A) + ((toInteger uint64B) `rotateL` 64)
    return uint128

readuint128 :: BS.ByteString -> Int -> Integer
readuint128 contents startpos = runGet getuint128 (BS.drop (fromIntegral startpos - 1) contents)

readfloat :: BS.ByteString -> Int -> Float
readfloat contents startpos = runGet getFloatle (BS.drop (fromIntegral startpos - 1) contents)

readfloatrow :: BS.ByteString -> Int -> Float
readfloatrow row startpos = runGet getFloatle (BS.drop (fromIntegral startpos) row)

readstr :: BS.ByteString -> Int -> String
readstr contents startpos = do
    let len = runGet getWord8 (BS.drop (fromIntegral startpos) contents)
    str <- BS8.unpack (BS.take (fromIntegral len) (BS.drop (fromIntegral startpos + 1) contents))
    return str

readcolcountry :: BS.ByteString -> Int -> Int -> [Int] -> (String, String)
readcolcountry contents dbtype rowoffset col = do
    let x = "This parameter is unavailable for selected data file. Please upgrade the data file."
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            (x, x)
        else do
            let coloffset = (colpos - 1) `shiftL` 2
            let x0 = readuint32 contents (rowoffset + coloffset)
            let x1 = readstr contents  x0
            let x2 = readstr contents (x0 + 3)
            (x1, x2)

readcolcountryrow :: BS.ByteString -> BS.ByteString -> Int -> [Int] -> (String, String)
readcolcountryrow contents row dbtype col = do
    let x = "This parameter is unavailable for selected data file. Please upgrade the data file."
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            (x, x)
        else do
            let coloffset = (colpos - 2) `shiftL` 2
            let x0 = readuint32row row coloffset
            let x1 = readstr contents  x0
            let x2 = readstr contents (x0 + 3)
            (x1, x2)

readcolstring :: BS.ByteString -> Int -> Int -> [Int] -> String
readcolstring contents dbtype rowoffset col = do
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            "This parameter is unavailable for selected data file. Please upgrade the data file."
        else do
            let coloffset = (colpos - 1) `shiftL` 2
            readstr contents (readuint32 contents (rowoffset + coloffset))

readcolstringrow :: BS.ByteString -> BS.ByteString -> Int -> [Int] -> String
readcolstringrow contents row dbtype col = do
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            "This parameter is unavailable for selected data file. Please upgrade the data file."
        else do
            let coloffset = (colpos - 2) `shiftL` 2
            readstr contents (readuint32row row coloffset)

readcolfloat :: BS.ByteString -> Int -> Int -> [Int] -> Float
readcolfloat contents dbtype rowoffset col = do
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            0.0
        else do
            let coloffset = (colpos - 1) `shiftL` 2
            readfloat contents (rowoffset + coloffset)

readcolfloatrow :: BS.ByteString -> Int -> [Int] -> Float
readcolfloatrow row dbtype col = do
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            0.0
        else do
            let coloffset = (colpos - 2) `shiftL` 2
            readfloatrow row coloffset

readcolfloatstring :: BS.ByteString -> Int -> Int -> [Int] -> Float
readcolfloatstring contents dbtype rowoffset col = do
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            0.0
        else do
            let coloffset = (colpos - 1) `shiftL` 2
            let n = readstr contents (readuint32 contents (rowoffset + coloffset))
            read n :: Float

readcolfloatstringrow :: BS.ByteString -> BS.ByteString -> Int -> [Int] -> Float
readcolfloatstringrow contents row dbtype col = do
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            0.0
        else do
            let coloffset = (colpos - 2) `shiftL` 2
            let n = readstr contents (readuint32row row coloffset)
            read n :: Float

countif :: (a -> Bool) -> [a] -> Int
countif f = length . filter f 

readrecord :: BS.ByteString -> Int -> Int -> IP2LocationRecord
readrecord contents dbtype rowoffset = do
    let country_position = [0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
    let region_position = [0, 0, 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]
    let city_position = [0, 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
    let isp_position = [0, 0, 3, 0, 5, 0, 7, 5, 7, 0, 8, 0, 9, 0, 9, 0, 9, 0, 9, 7, 9, 0, 9, 7, 9]
    let latitude_position = [0, 0, 0, 0, 0, 5, 5, 0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
    let longitude_position = [0, 0, 0, 0, 0, 6, 6, 0, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6]
    let domain_position = [0, 0, 0, 0, 0, 0, 0, 6, 8, 0, 9, 0, 10,0, 10, 0, 10, 0, 10, 8, 10, 0, 10, 8, 10]
    let zipcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 7, 7, 7, 0, 7, 7, 7, 0, 7, 0, 7, 7, 7, 0, 7]
    let timezone_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 7, 8, 8, 8, 7, 8, 0, 8, 8, 8, 0, 8]
    let netspeed_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 11,0, 11,8, 11, 0, 11, 0, 11, 0, 11]
    let iddcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 12, 0, 12, 0, 12, 9, 12, 0, 12]
    let areacode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10 ,13 ,0, 13, 0, 13, 10, 13, 0, 13]
    let weatherstationcode_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 14, 0, 14, 0, 14, 0, 14]
    let weatherstationname_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 15, 0, 15, 0, 15, 0, 15]
    let mcc_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 16, 0, 16, 9, 16]
    let mnc_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10,17, 0, 17, 10, 17]
    let mobilebrand_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11,18, 0, 18, 11, 18]
    let elevation_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 19, 0, 19]
    let usagetype_position = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12, 20]
    
    let allcols = (take 1 (drop dbtype country_position)) ++ (take 1 (drop dbtype region_position)) ++ (take 1 (drop dbtype city_position)) ++ (take 1 (drop dbtype isp_position)) ++ (take 1 (drop dbtype latitude_position)) ++ (take 1 (drop dbtype longitude_position)) ++ (take 1 (drop dbtype domain_position)) ++ (take 1 (drop dbtype zipcode_position)) ++ (take 1 (drop dbtype timezone_position)) ++ (take 1 (drop dbtype netspeed_position)) ++ (take 1 (drop dbtype iddcode_position)) ++ (take 1 (drop dbtype areacode_position)) ++ (take 1 (drop dbtype weatherstationcode_position)) ++ (take 1 (drop dbtype weatherstationname_position)) ++ (take 1 (drop dbtype mcc_position)) ++ (take 1 (drop dbtype mnc_position)) ++ (take 1 (drop dbtype mobilebrand_position)) ++ (take 1 (drop dbtype elevation_position)) ++ (take 1 (drop dbtype usagetype_position))
    let cols = (countif (>0) allcols) `shiftL` 2
    let row = BS.take (fromIntegral cols) (BS.drop (fromIntegral rowoffset - 1) contents)
    
    -- let (country_short, country_long) = readcolcountry contents dbtype rowoffset country_position
    -- let region = readcolstring contents dbtype rowoffset region_position
    -- let city = readcolstring contents dbtype rowoffset city_position
    -- let isp = readcolstring contents dbtype rowoffset isp_position
    -- let latitude = readcolfloat contents dbtype rowoffset latitude_position
    -- let longitude = readcolfloat contents dbtype rowoffset longitude_position
    -- let domain = readcolstring contents dbtype rowoffset domain_position
    -- let zipcode = readcolstring contents dbtype rowoffset zipcode_position
    -- let timezone = readcolstring contents dbtype rowoffset timezone_position
    -- let netspeed = readcolstring contents dbtype rowoffset netspeed_position
    -- let iddcode = readcolstring contents dbtype rowoffset iddcode_position
    -- let areacode = readcolstring contents dbtype rowoffset areacode_position
    -- let weatherstationcode = readcolstring contents dbtype rowoffset weatherstationcode_position
    -- let weatherstationname = readcolstring contents dbtype rowoffset weatherstationname_position
    -- let mcc = readcolstring contents dbtype rowoffset mcc_position
    -- let mnc = readcolstring contents dbtype rowoffset mnc_position
    -- let mobilebrand = readcolstring contents dbtype rowoffset mobilebrand_position
    -- let elevation = readcolfloatstring contents dbtype rowoffset elevation_position
    -- let usagetype = readcolstring contents dbtype rowoffset usagetype_position
    
    let (country_short, country_long) = readcolcountryrow contents row dbtype country_position
    let region = readcolstringrow contents row dbtype region_position
    let city = readcolstringrow contents row dbtype city_position
    let isp = readcolstringrow contents row dbtype isp_position
    let latitude = readcolfloatrow row dbtype latitude_position
    let longitude = readcolfloatrow row dbtype longitude_position
    let domain = readcolstringrow contents row dbtype domain_position
    let zipcode = readcolstringrow contents row dbtype zipcode_position
    let timezone = readcolstringrow contents row dbtype timezone_position
    let netspeed = readcolstringrow contents row dbtype netspeed_position
    let iddcode = readcolstringrow contents row dbtype iddcode_position
    let areacode = readcolstringrow contents row dbtype areacode_position
    let weatherstationcode = readcolstringrow contents row dbtype weatherstationcode_position
    let weatherstationname = readcolstringrow contents row dbtype weatherstationname_position
    let mcc = readcolstringrow contents row dbtype mcc_position
    let mnc = readcolstringrow contents row dbtype mnc_position
    let mobilebrand = readcolstringrow contents row dbtype mobilebrand_position
    let elevation = readcolfloatstringrow contents row dbtype elevation_position
    let usagetype = readcolstringrow contents row dbtype usagetype_position
    
    IP2LocationRecord country_short country_long region city isp latitude longitude domain zipcode timezone netspeed iddcode areacode weatherstationcode weatherstationname mcc mnc mobilebrand elevation usagetype 

searchtree :: BS.ByteString -> Integer -> Int -> Int -> Int -> Int -> Int -> Int -> IP2LocationRecord
searchtree contents ipnum dbtype low high baseaddr colsize iptype = do
    if low <= high
        then do
            let mid = ((low + high) `shiftR` 1)
            let rowoffset = baseaddr + (mid * colsize)
            let rowoffset2 = rowoffset + colsize
            
            let ipfrom = if (iptype == 4)
                then toInteger $ readuint32 contents rowoffset
                else readuint128 contents rowoffset
            
            let ipto = if (iptype == 4)
                then toInteger $ readuint32 contents rowoffset2
                else readuint128 contents rowoffset2
            
            if ipnum >= ipfrom && ipnum < ipto
                then do
                    if iptype == 4
                        then
                            -- readrecord contents dbtype rowoffset
                            readrecord contents dbtype (rowoffset + 4)
                        else
                            -- readrecord contents dbtype (rowoffset + 12)
                            readrecord contents dbtype (rowoffset + 16)
                else if ipnum < ipfrom
                    then
                        searchtree contents ipnum dbtype low (mid - 1) baseaddr colsize iptype
                    else
                        searchtree contents ipnum dbtype (mid + 1) high baseaddr colsize iptype
        else do
            let x = "IP address not found."
            IP2LocationRecord x x x x x 0.0 0.0 x x x x x x x x x x x 0.0 x 
        
search4 :: BS.ByteString -> Integer -> Int -> Int -> Int -> Int -> Int -> Int -> IP2LocationRecord
search4 contents ipnum dbtype low high baseaddr indexbaseaddr colsize = do
    if indexbaseaddr > 0
        then do
            let indexpos = fromIntegral (((ipnum `rotateR` 16) `rotateL` 3) + (toInteger indexbaseaddr))
            let low2 = readuint32 contents indexpos
            let high2 = readuint32 contents (indexpos + 4)
            searchtree contents ipnum dbtype low2 high2 baseaddr colsize 4
        else
            searchtree contents ipnum dbtype low high baseaddr colsize 4

search6 :: BS.ByteString -> Integer -> Int -> Int -> Int -> Int -> Int -> Int -> IP2LocationRecord
search6 contents ipnum dbtype low high baseaddr indexbaseaddr colsize = do
    if indexbaseaddr > 0
        then do
            let indexpos = fromIntegral (((ipnum `rotateR` 112) `rotateL` 3) + (toInteger indexbaseaddr))
            let low2 = readuint32 contents indexpos
            let high2 = readuint32 contents (indexpos + 4)
            searchtree contents ipnum dbtype low2 high2 baseaddr colsize 6
        else
            searchtree contents ipnum dbtype low high baseaddr colsize 6

tryfirst myIP = do
    result <- try (evaluate (ipStringToInteger myIP)) :: IO (Either SomeException Integer)
    case result of
        Left ex -> return $ toInteger (1 - 2)
        Right val -> return val

{-|
    The 'doQuery' function returns an IP2LocationRecord containing geolocation data for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'doInit' function (Meta record) & either IPv4 or IPv6 address (String).
-}
doQuery :: String -> Meta -> String -> IO IP2LocationRecord
doQuery myfile meta myip = do
    contents <- BS.readFile myfile
    let fromV4Mapped = 281470681743360
    let toV4Mapped = 281474976710655
    let fromV4Compatible = 0
    let toV4Compatible = 4294967295
    let from6To4 = 42545680458834377588178886921629466624
    let to6To4 = 42550872755692912415807417417958686719
    let fromTeredo = 42540488161975842760550356425300246528
    let toTeredo = 42540488241204005274814694018844196863
    let last32Bits = 4294967295
    
    ipnum <- tryfirst myip
    if ipnum == -1
        then do
            let x = "Invalid IP address."
            return $ IP2LocationRecord x x x x x 0.0 0.0 x x x x x x x x x x x 0.0 x
        else if ipnum >= fromV4Mapped && ipnum <= toV4Mapped
            then do
                return $ search4 contents (ipnum - (toInteger fromV4Mapped)) (databasetype meta) 0 (ipv4databasecount meta) (ipv4databaseaddr meta) (ipv4indexbaseaddr meta) (ipv4columnsize meta)
            else if ipnum >= from6To4 && ipnum <= to6To4
                then do
                    return $ search4 contents ((ipnum `rotateR` 80) .&. last32Bits) (databasetype meta) 0 (ipv4databasecount meta) (ipv4databaseaddr meta) (ipv4indexbaseaddr meta) (ipv4columnsize meta)
                else if ipnum >= fromTeredo && ipnum <= toTeredo
                    then do
                        return $ search4 contents ((complement ipnum) .&. last32Bits) (databasetype meta) 0 (ipv4databasecount meta) (ipv4databaseaddr meta) (ipv4indexbaseaddr meta) (ipv4columnsize meta)
                    else if ipnum >= fromV4Compatible && ipnum <= toV4Compatible
                        then do
                            return $ search4 contents ipnum (databasetype meta) 0 (ipv4databasecount meta) (ipv4databaseaddr meta) (ipv4indexbaseaddr meta) (ipv4columnsize meta)
                        else do
                            return $ search6 contents ipnum (databasetype meta) 0 (ipv6databasecount meta) (ipv6databaseaddr meta) (ipv6indexbaseaddr meta) (ipv6columnsize meta)
