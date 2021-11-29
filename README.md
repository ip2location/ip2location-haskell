# IP2Location Haskell Package

This Haskell package provides a fast lookup of country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, station name, mcc, mnc, mobile brand, elevation, usage type, address type and IAB category from IP address by using IP2Location database. This package uses a file based database available at IP2Location.com. This database simply contains IP blocks as keys, and other information such as country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, station name, mcc, mnc, mobile brand, elevation, usage type, address type and IAB category as values. It supports both IP address in IPv4 and IPv6.

This package can be used in many types of projects such as:

 - select the geographically closest mirror
 - analyze your web server logs to determine the countries of your visitors
 - credit card fraud detection
 - software export controls
 - display native language and currency 
 - prevent password sharing and abuse of service 
 - geotargeting in advertisement

The database will be updated in monthly basis for the greater accuracy. Free LITE databases are available at https://lite.ip2location.com/ upon registration.

The paid databases are available at https://www.ip2location.com under Premium subscription package.

As an alternative, this package can also call the IP2Location Web Service. This requires an API key. If you don't have an existing API key, you can subscribe for one at the below:

https://www.ip2location.com/web-service/ip2location

## Installation

```
cabal install IP2Location
```

## QUERY USING THE BIN FILE

## Dependencies

This package requires IP2Location BIN data file to function. You may download the BIN data file at
* IP2Location LITE BIN Data (Free): https://lite.ip2location.com
* IP2Location Commercial BIN Data (Comprehensive): https://www.ip2location.com


## IPv4 BIN vs IPv6 BIN

Use the IPv4 BIN file if you just need to query IPv4 addresses.

Use the IPv6 BIN file if you need to query BOTH IPv4 and IPv6 addresses.


## Methods

Below are the methods supported in this package.

|Method Name|Description|
|---|---|
|doInit|Initialize with the BIN file.|
|doQuery|Return geolocation data by IP address.|

## Usage

```haskell
import IP2Location

main :: IO ()
main = do
    let myfile = "IPV6-COUNTRY-REGION-CITY-LATITUDE-LONGITUDE-ZIPCODE-TIMEZONE-ISP-DOMAIN-NETSPEED-AREACODE-WEATHER-MOBILE-ELEVATION-USAGETYPE-ADDRESSTYPE-CATEGORY.BIN"
    meta <- doInit myfile
    result <- doQuery myfile meta "8.8.8.8"
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
```

## QUERY USING THE IP2LOCATION WEB SERVICE

## Methods
Below are the methods supported in this package.

|Method Name|Description|
|---|---|
|openWS| 3 input parameters:<ol><li>IP2Location API Key.</li><li>Package (WS1 - WS25)</li></li><li>Use HTTPS or HTTP</li></ol> |
|lookUp|Query IP address. This method returns an object containing the geolocation info. <ul><li>country_code</li><li>country_name</li><li>region_name</li><li>city_name</li><li>latitude</li><li>longitude</li><li>zip_code</li><li>time_zone</li><li>isp</li><li>domain</li><li>net_speed</li><li>idd_code</li><li>area_code</li><li>weather_station_code</li><li>weather_station_name</li><li>mcc</li><li>mnc</li><li>mobile_brand</li><li>elevation</li><li>usage_type</li><li>address_type</li><li>category</li><ul>|
|getCredit|This method returns the web service credit balance.|

## Usage

```haskell
import IP2LocationWebService
import Data.Maybe

main :: IO ()
main = do
    let apikey = "YOUR_API_KEY"
    let apipackage = "WS25"
    let usessl = True
    let ip = "8.8.8.8"
    wsconfig <- openWS apikey apipackage usessl
    result <- lookUp wsconfig ip
    putStrLn $ "response: " ++ (response result)
    putStrLn $ "country_code: " ++ (fromMaybe ("-") $ (country_code result))
    putStrLn $ "country_name: " ++ (fromMaybe ("-") $ (country_name result))
    putStrLn $ "region_name: " ++ (fromMaybe ("-") $ (region_name result))
    putStrLn $ "city_name: " ++ (fromMaybe ("-") $ (city_name result))
    putStrLn $ "latitude: " ++ show (fromMaybe (0.0) $ (latitude result))
    putStrLn $ "longitude: " ++ show (fromMaybe (0.0) $ (longitude result))
    putStrLn $ "zip_code: " ++ (fromMaybe ("-") $ (zip_code result))
    putStrLn $ "time_zone: " ++ (fromMaybe ("-") $ (time_zone result))
    putStrLn $ "isp: " ++ (fromMaybe ("-") $ (isp result))
    putStrLn $ "domain: " ++ (fromMaybe ("-") $ (domain result))
    putStrLn $ "net_speed: " ++ (fromMaybe ("-") $ (net_speed result))
    putStrLn $ "idd_code: " ++ (fromMaybe ("-") $ (idd_code result))
    putStrLn $ "area_code: " ++ (fromMaybe ("-") $ (area_code result))
    putStrLn $ "weather_station_code: " ++ (fromMaybe ("-") $ (weather_station_code result))
    putStrLn $ "weather_station_name: " ++ (fromMaybe ("-") $ (weather_station_name result))
    putStrLn $ "mcc: " ++ (fromMaybe ("-") $ (mcc result))
    putStrLn $ "mnc: " ++ (fromMaybe ("-") $ (mnc result))
    putStrLn $ "mobile_brand: " ++ (fromMaybe ("-") $ (mobile_brand result))
    putStrLn $ "elevation: " ++ show (fromMaybe (0.0) $ (elevation result))
    putStrLn $ "usage_type: " ++ (fromMaybe ("-") $ (usage_type result))
    putStrLn $ "address_type: " ++ (fromMaybe ("-") $ (address_type result))
    putStrLn $ "category: " ++ (fromMaybe ("-") $ (category result))
    putStrLn $ "category_name: " ++ (fromMaybe ("-") $ (category_name result))
    putStrLn $ "credits_consumed: " ++ show (fromMaybe (0.0) $ (credits_consumed result))
    result2 <- getCredit wsconfig
    putStrLn $ "Credit Balance: " ++ result2
```