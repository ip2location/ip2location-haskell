# Quickstart

## Dependencies

This library requires IP2Location BIN database to function. You may download the BIN database at

-   IP2Location LITE BIN Data (Free): <https://lite.ip2location.com>
-   IP2Location Commercial BIN Data (Comprehensive):
    <https://www.ip2location.com>

## IPv4 BIN vs IPv6 BIN

Use the IPv4 BIN file if you just need to query IPv4 addresses.

Use the IPv6 BIN file if you need to query BOTH IPv4 and IPv6 addresses.

## Installation

To install this module type the following:

```bash
cabal install IP2Location
```

## Sample Codes

### Query geolocation information from BIN database

You can query the geolocation information from the IP2Location BIN database as below:

```haskell
import IP2Location
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    let myfile = "IPV6-COUNTRY-REGION-CITY-LATITUDE-LONGITUDE-ZIPCODE-TIMEZONE-ISP-DOMAIN-NETSPEED-AREACODE-WEATHER-MOBILE-ELEVATION-USAGETYPE-ADDRESSTYPE-CATEGORY-DISTRICT-ASN.BIN"
    contents <- BS.readFile myfile -- load BIN once
    meta <- doInitBS contents
    result <- doQueryBS contents meta "8.8.8.8"
    putStrLn $ "country_short: " ++ (show (country_short result))
    putStrLn $ "country_long: " ++ (show (country_long result))
    putStrLn $ "region: " ++ (show (region result))
    putStrLn $ "city: " ++ (show (city result))
    putStrLn $ "isp: " ++ (show (isp result))
    putStrLn $ "latitude: " ++ (show (latitude result))
    putStrLn $ "longitude: " ++ (show (longitude result))
    putStrLn $ "domain: " ++ (show (domain result))
    putStrLn $ "zipcode: " ++ (show (zipcode result))
    putStrLn $ "timezone: " ++ (show (timezone result))
    putStrLn $ "netspeed: " ++ (show (netspeed result))
    putStrLn $ "iddcode: " ++ (show (iddcode result))
    putStrLn $ "areacode: " ++ (show (areacode result))
    putStrLn $ "weatherstationcode: " ++ (show (weatherstationcode result))
    putStrLn $ "weatherstationname: " ++ (show (weatherstationname result))
    putStrLn $ "mcc: " ++ (show (mcc result))
    putStrLn $ "mnc: " ++ (show (mnc result))
    putStrLn $ "mobilebrand: " ++ (show (mobilebrand result))
    putStrLn $ "elevation: " ++ (show (elevation result))
    putStrLn $ "usagetype: " ++ (show (usagetype result))
    putStrLn $ "addresstype: " ++ (show (addresstype result))
    putStrLn $ "category: " ++ (show (category result))
    putStrLn $ "district: " ++ (show (district result))
    putStrLn $ "asn: " ++ (show (asn result))
    putStrLn $ "as: " ++ (show (as result))
    putStrLn $ "asdomain: " ++ (show (asdomain result))
    putStrLn $ "asusagetype: " ++ (show (asusagetype result))
    putStrLn $ "ascidr: " ++ (show (ascidr result))
```