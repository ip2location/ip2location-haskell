IP2Location Haskell Package
===========================

This Haskell package provides a fast lookup of country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, station name, mcc, mnc, mobile brand, elevation, and usage type from IP address by using IP2Location database. This package uses a file based database available at IP2Location.com. This database simply contains IP blocks as keys, and other information such as country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain name, connection type, IDD code, area code, weather station code, station name, mcc, mnc, mobile brand, elevation, and usage type as values. It supports both IP address in IPv4 and IPv6.

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


Installation
=======

```
cabal install IP2Location
```

Example
=======

```haskell
import IP2Location

main :: IO ()
main = do
    let myfile = "IPV6-COUNTRY-REGION-CITY-LATITUDE-LONGITUDE-ZIPCODE-TIMEZONE-ISP-DOMAIN-NETSPEED-AREACODE-WEATHER-MOBILE-ELEVATION-USAGETYPE.BIN"
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
```

Dependencies
============

The complete database is available at https://www.ip2location.com under subscription package.


IPv4 BIN vs IPv6 BIN
====================
Use the IPv4 BIN file if you just need to query IPv4 addresses.

Use the IPv6 BIN file if you need to query BOTH IPv4 and IPv6 addresses.


Copyright
=========

Copyright (C) 2019 by IP2Location.com, support@ip2location.com
