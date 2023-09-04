IP2Location.io Erlang SDK
=========================
This Erlang module enables user to query for an enriched data set, such as country, region, district, city, latitude & longitude, ZIP code, time zone, ASN, ISP, domain, net speed, IDD code, area code, weather station data, MNC, MCC, mobile brand, elevation, usage type, address type, advertisement category and proxy data with an IP address. It supports both IPv4 and IPv6 address lookup.

In addition, this module provides WHOIS lookup api that helps users to obtain domain information, WHOIS record, by using a domain name. The WHOIS API returns a comprehensive WHOIS data such as creation date, updated date, expiration date, domain age, the contact information of the registrant, mailing address, phone number, email address, nameservers the domain is using and much more.

This module requires API key to function. You may sign up for a free API key at https://www.ip2location.io/pricing.


Compilation
============
```bash

erlc ip2locationio.erl
erlc test.erl

```


Usage Example
============
### Lookup IP Address Geolocation Data
```erlang
-module(test).
-export([testme/0]).

testme() ->
	APIKey = "YOUR_API_KEY",
	IP = "8.8.8.8",
	Lang = "es",
	
	ip2locationio:new(APIKey),
	Result = ip2locationio:lookupipgeolocation(IP, Lang), % Lang only supported in Plus and Security Plans so omit if not needed
	case Result of
		{error, Reason} ->
			io:format("Error: ~p~n", [Reason]);
		_ ->
			% standard results
			io:format("ip: ~p~n", [maps:get(<<"ip">>, Result)]),
			io:format("country_code: ~p~n", [maps:get(<<"country_code">>, Result)]),
			io:format("country_name: ~p~n", [maps:get(<<"country_name">>, Result)]),
			io:format("region_name: ~p~n", [maps:get(<<"region_name">>, Result)]),
			io:format("city_name: ~p~n", [maps:get(<<"city_name">>, Result)]),
			io:format("latitude: ~p~n", [maps:get(<<"latitude">>, Result)]),
			io:format("longitude: ~p~n", [maps:get(<<"longitude">>, Result)]),
			io:format("zip_code: ~p~n", [maps:get(<<"zip_code">>, Result)]),
			io:format("time_zone: ~p~n", [maps:get(<<"time_zone">>, Result)]),
			io:format("asn: ~p~n", [maps:get(<<"asn">>, Result)]),
			io:format("as: ~p~n", [maps:get(<<"as">>, Result)]),
			io:format("isp: ~p~n", [maps:get(<<"isp">>, Result)]),
			io:format("domain: ~p~n", [maps:get(<<"domain">>, Result)]),
			io:format("net_speed: ~p~n", [maps:get(<<"net_speed">>, Result)]),
			io:format("idd_code: ~p~n", [maps:get(<<"idd_code">>, Result)]),
			io:format("area_code: ~p~n", [maps:get(<<"area_code">>, Result)]),
			io:format("weather_station_code: ~p~n", [maps:get(<<"weather_station_code">>, Result)]),
			io:format("weather_station_name: ~p~n", [maps:get(<<"weather_station_name">>, Result)]),
			io:format("mcc: ~p~n", [maps:get(<<"mcc">>, Result)]),
			io:format("mnc: ~p~n", [maps:get(<<"mnc">>, Result)]),
			io:format("mobile_brand: ~p~n", [maps:get(<<"mobile_brand">>, Result)]),
			io:format("elevation: ~p~n", [maps:get(<<"elevation">>, Result)]),
			io:format("usage_type: ~p~n", [maps:get(<<"usage_type">>, Result)]),
			io:format("address_type: ~p~n", [maps:get(<<"address_type">>, Result)]),
			io:format("district: ~p~n", [maps:get(<<"district">>, Result)]),
			io:format("ads_category: ~p~n", [maps:get(<<"ads_category">>, Result)]),
			io:format("ads_category_name: ~p~n", [maps:get(<<"ads_category_name">>, Result)]),
			io:format("is_proxy: ~p~n", [maps:get(<<"is_proxy">>, Result)]),
			
			% continent addon
			case maps:is_key(<<"continent">>, Result) of
				true ->
					Continent = maps:get(<<"continent">>, Result),
					io:format("continent => name: ~p~n", [maps:get(<<"name">>, Continent)]),
					io:format("continent => code: ~p~n", [maps:get(<<"code">>, Continent)]),
					io:format("continent => hemisphere: ~p~n", [maps:get(<<"hemisphere">>, Continent)]),
					io:format("continent => translation: ~p~n", [maps:get(<<"translation">>, Continent)]);
				_ ->
					""
			end,
			
			% country addon
			case maps:is_key(<<"country">>, Result) of
				true ->
					Country = maps:get(<<"country">>, Result),
					io:format("country => name: ~p~n", [maps:get(<<"name">>, Country)]),
					io:format("country => alpha3_code: ~p~n", [maps:get(<<"alpha3_code">>, Country)]),
					io:format("country => numeric_code: ~p~n", [maps:get(<<"numeric_code">>, Country)]),
					io:format("country => demonym: ~p~n", [maps:get(<<"demonym">>, Country)]),
					io:format("country => flag: ~p~n", [maps:get(<<"flag">>, Country)]),
					io:format("country => capital: ~p~n", [maps:get(<<"capital">>, Country)]),
					io:format("country => total_area: ~p~n", [maps:get(<<"total_area">>, Country)]),
					io:format("country => population: ~p~n", [maps:get(<<"population">>, Country)]),
					io:format("country => tld: ~p~n", [maps:get(<<"tld">>, Country)]),
					
					CountryCurrency = maps:get(<<"currency">>, Country),
					io:format("country => currency => code: ~p~n", [maps:get(<<"code">>, CountryCurrency)]),
					io:format("country => currency => name: ~p~n", [maps:get(<<"name">>, CountryCurrency)]),
					io:format("country => currency => symbol: ~p~n", [maps:get(<<"symbol">>, CountryCurrency)]),
					
					CountryLanguage = maps:get(<<"language">>, Country),
					io:format("country => language => code: ~p~n", [maps:get(<<"code">>, CountryLanguage)]),
					io:format("country => language => name: ~p~n", [maps:get(<<"name">>, CountryLanguage)]),
					
					io:format("country => translation: ~p~n", [maps:get(<<"translation">>, Country)]);
				_ ->
					""
			end,
			
			% region addon
			case maps:is_key(<<"region">>, Result) of
				true ->
					Region = maps:get(<<"region">>, Result),
					io:format("region => name: ~p~n", [maps:get(<<"name">>, Region)]),
					io:format("region => code: ~p~n", [maps:get(<<"code">>, Region)]),
					io:format("region => translation: ~p~n", [maps:get(<<"translation">>, Region)]);
				_ ->
					""
			end,
			
			% city addon
			case maps:is_key(<<"city">>, Result) of
				true ->
					City = maps:get(<<"city">>, Result),
					io:format("city => name: ~p~n", [maps:get(<<"name">>, City)]),
					io:format("city => translation: ~p~n", [maps:get(<<"translation">>, City)]);
				_ ->
					""
			end,
			
			% time_zone_info addon
			case maps:is_key(<<"time_zone_info">>, Result) of
				true ->
					TimeZone = maps:get(<<"time_zone_info">>, Result),
					io:format("time_zone_info => olson: ~p~n", [maps:get(<<"olson">>, TimeZone)]),
					io:format("time_zone_info => current_time: ~p~n", [maps:get(<<"current_time">>, TimeZone)]),
					io:format("time_zone_info => gmt_offset: ~p~n", [maps:get(<<"gmt_offset">>, TimeZone)]),
					io:format("time_zone_info => is_dst: ~p~n", [maps:get(<<"is_dst">>, TimeZone)]),
					io:format("time_zone_info => sunrise: ~p~n", [maps:get(<<"sunrise">>, TimeZone)]),
					io:format("time_zone_info => sunset: ~p~n", [maps:get(<<"sunset">>, TimeZone)]);
				_ ->
					""
			end,
			
			% geotargeting addon
			case maps:is_key(<<"geotargeting">>, Result) of
				true ->
					Geotargeting = maps:get(<<"geotargeting">>, Result),
					io:format("geotargeting => metro: ~p~n", [maps:get(<<"metro">>, Geotargeting)]);
				_ ->
					""
			end,
			
			% proxy addon
			case maps:is_key(<<"proxy">>, Result) of
				true ->
					Proxy = maps:get(<<"proxy">>, Result),
					io:format("proxy => last_seen: ~p~n", [maps:get(<<"last_seen">>, Proxy)]),
					io:format("proxy => proxy_type: ~p~n", [maps:get(<<"proxy_type">>, Proxy)]),
					io:format("proxy => threat: ~p~n", [maps:get(<<"threat">>, Proxy)]),
					io:format("proxy => provider: ~p~n", [maps:get(<<"provider">>, Proxy)]),
					io:format("proxy => is_vpn: ~p~n", [maps:get(<<"is_vpn">>, Proxy)]),
					io:format("proxy => is_tor: ~p~n", [maps:get(<<"is_tor">>, Proxy)]),
					io:format("proxy => is_data_center: ~p~n", [maps:get(<<"is_data_center">>, Proxy)]),
					io:format("proxy => is_public_proxy: ~p~n", [maps:get(<<"is_public_proxy">>, Proxy)]),
					io:format("proxy => is_web_proxy: ~p~n", [maps:get(<<"is_web_proxy">>, Proxy)]),
					io:format("proxy => is_web_crawler: ~p~n", [maps:get(<<"is_web_crawler">>, Proxy)]),
					io:format("proxy => is_residential_proxy: ~p~n", [maps:get(<<"is_residential_proxy">>, Proxy)]),
					io:format("proxy => is_spammer: ~p~n", [maps:get(<<"is_spammer">>, Proxy)]),
					io:format("proxy => is_scanner: ~p~n", [maps:get(<<"is_scanner">>, Proxy)]),
					io:format("proxy => is_botnet: ~p~n", [maps:get(<<"is_botnet">>, Proxy)]);
				_ ->
					""
			end
	end.
```

### Lookup Domain Information
```erlang
-module(test).
-export([testme/0]).

testme() ->
	APIKey = "YOUR_API_KEY",
	
	ip2locationio:new(APIKey),
	Domain = "locaproxy.com",
	Result = ip2locationio:lookupdomainwhois(Domain),
	case Result of
		{error, Reason2} ->
			io:format("Error: ~p~n", [Reason2]);
		_ ->
			io:format("domain: ~p~n", [maps:get(<<"domain">>, Result)]),
			io:format("domain_id: ~p~n", [maps:get(<<"domain_id">>, Result)]),
			io:format("status: ~p~n", [maps:get(<<"status">>, Result)]),
			io:format("create_date: ~p~n", [maps:get(<<"create_date">>, Result)]),
			io:format("update_date: ~p~n", [maps:get(<<"update_date">>, Result)]),
			io:format("expire_date: ~p~n", [maps:get(<<"expire_date">>, Result)]),
			io:format("domain_age: ~p~n", [maps:get(<<"domain_age">>, Result)]),
			io:format("whois_server: ~p~n", [maps:get(<<"whois_server">>, Result)]),
			io:format("nameservers: ~p~n", [maps:get(<<"nameservers">>, Result)]),
			
			Registrar = maps:get(<<"registrar">>, Result),
			io:format("registrar => iana_id: ~p~n", [maps:get(<<"iana_id">>, Registrar)]),
			io:format("registrar => name: ~p~n", [maps:get(<<"name">>, Registrar)]),
			io:format("registrar => url: ~p~n", [maps:get(<<"url">>, Registrar)]),
			
			Registrant = maps:get(<<"registrant">>, Result),
			io:format("registrant => name: ~p~n", [maps:get(<<"name">>, Registrant)]),
			io:format("registrant => organization: ~p~n", [maps:get(<<"organization">>, Registrant)]),
			io:format("registrant => street_address: ~p~n", [maps:get(<<"street_address">>, Registrant)]),
			io:format("registrant => city: ~p~n", [maps:get(<<"city">>, Registrant)]),
			io:format("registrant => region: ~p~n", [maps:get(<<"region">>, Registrant)]),
			io:format("registrant => zip_code: ~p~n", [maps:get(<<"zip_code">>, Registrant)]),
			io:format("registrant => country: ~p~n", [maps:get(<<"country">>, Registrant)]),
			io:format("registrant => phone: ~p~n", [maps:get(<<"phone">>, Registrant)]),
			io:format("registrant => fax: ~p~n", [maps:get(<<"fax">>, Registrant)]),
			io:format("registrant => email: ~p~n", [maps:get(<<"email">>, Registrant)]),
			
			Admin = maps:get(<<"admin">>, Result),
			io:format("admin => name: ~p~n", [maps:get(<<"name">>, Admin)]),
			io:format("admin => organization: ~p~n", [maps:get(<<"organization">>, Admin)]),
			io:format("admin => street_address: ~p~n", [maps:get(<<"street_address">>, Admin)]),
			io:format("admin => city: ~p~n", [maps:get(<<"city">>, Admin)]),
			io:format("admin => region: ~p~n", [maps:get(<<"region">>, Admin)]),
			io:format("admin => zip_code: ~p~n", [maps:get(<<"zip_code">>, Admin)]),
			io:format("admin => country: ~p~n", [maps:get(<<"country">>, Admin)]),
			io:format("admin => phone: ~p~n", [maps:get(<<"phone">>, Admin)]),
			io:format("admin => fax: ~p~n", [maps:get(<<"fax">>, Admin)]),
			io:format("admin => email: ~p~n", [maps:get(<<"email">>, Admin)]),
			
			Tech = maps:get(<<"tech">>, Result),
			io:format("tech => name: ~p~n", [maps:get(<<"name">>, Tech)]),
			io:format("tech => organization: ~p~n", [maps:get(<<"organization">>, Tech)]),
			io:format("tech => street_address: ~p~n", [maps:get(<<"street_address">>, Tech)]),
			io:format("tech => city: ~p~n", [maps:get(<<"city">>, Tech)]),
			io:format("tech => region: ~p~n", [maps:get(<<"region">>, Tech)]),
			io:format("tech => zip_code: ~p~n", [maps:get(<<"zip_code">>, Tech)]),
			io:format("tech => country: ~p~n", [maps:get(<<"country">>, Tech)]),
			io:format("tech => phone: ~p~n", [maps:get(<<"phone">>, Tech)]),
			io:format("tech => fax: ~p~n", [maps:get(<<"fax">>, Tech)]),
			io:format("tech => email: ~p~n", [maps:get(<<"email">>, Tech)]),
			
			Billing = maps:get(<<"billing">>, Result),
			io:format("billing => name: ~p~n", [maps:get(<<"name">>, Billing)]),
			io:format("billing => organization: ~p~n", [maps:get(<<"organization">>, Billing)]),
			io:format("billing => street_address: ~p~n", [maps:get(<<"street_address">>, Billing)]),
			io:format("billing => city: ~p~n", [maps:get(<<"city">>, Billing)]),
			io:format("billing => region: ~p~n", [maps:get(<<"region">>, Billing)]),
			io:format("billing => zip_code: ~p~n", [maps:get(<<"zip_code">>, Billing)]),
			io:format("billing => country: ~p~n", [maps:get(<<"country">>, Billing)]),
			io:format("billing => phone: ~p~n", [maps:get(<<"phone">>, Billing)]),
			io:format("billing => fax: ~p~n", [maps:get(<<"fax">>, Billing)]),
			io:format("billing => email: ~p~n", [maps:get(<<"email">>, Billing)])
	end.
```

### Convert Normal Text to Punycode
```erlang
-module(test).
-export([testme/0]).

testme() ->
	APIKey = "YOUR_API_KEY",
	
	ip2locationio:new(APIKey),
	
	Punycode = ip2locationio:getpunycode("täst.de"),
	io:format("Punycode: ~p~n", [Punycode]).
```

### Convert Punycode to Normal Text
```erlang
-module(test).
-export([testme/0]).

testme() ->
	APIKey = "YOUR_API_KEY",
	
	ip2locationio:new(APIKey),
	
	Normaltext = ip2locationio:getnormaltext("xn--tst-qla.de"),
	io:format("Normaltext: ~p~n", [Normaltext]).
```

### Get Domain Name
```erlang
-module(test).
-export([testme/0]).

testme() ->
	APIKey = "YOUR_API_KEY",
	
	ip2locationio:new(APIKey),
	
	Domainname = ip2locationio:getdomainname("https://www.example.com/exe"),
	io:format("Domainname: ~p~n", [Domainname]).
```

### Get Domain Extension
```erlang
-module(test).
-export([testme/0]).

testme() ->
	APIKey = "YOUR_API_KEY",
	
	ip2locationio:new(APIKey),
	
	Domainextension = ip2locationio:getdomainextension("example.com"),
	io:format("Domainextension: ~p~n", [Domainextension]).
```


Response Parameter
============
### IP Geolocation Lookup function
| Parameter | Type | Description |
|---|---|---|
|ip|string|IP address.|
|country_code|string|Two-character country code based on ISO 3166.|
|country_name|string|Country name based on ISO 3166.|
|region_name|string|Region or state name.|
|city_name|string|City name.|
|latitude|double|City latitude. Defaults to capital city latitude if city is unknown.|
|longitude|double|City longitude. Defaults to capital city longitude if city is unknown.|
|zip_code|string|ZIP/Postal code.|
|time_zone|string|UTC time zone (with DST supported).|
|asn|string|Autonomous system number (ASN).|
|as|string|Autonomous system (AS) name.|
|isp|string|Internet Service Provider or company's name.|
|domain|string|Internet domain name associated with IP address range.|
|net_speed|string|Internet connection type. DIAL = dial-up, DSL = broadband/cable/fiber/mobile, COMP = company/T1|
|idd_code|string|The IDD prefix to call the city from another country.|
|area_code|string|A varying length number assigned to geographic areas for calls between cities.|
|weather_station_code|string|The special code to identify the nearest weather observation station.|
|weather_station_name|string|The name of the nearest weather observation station.|
|mcc|string|Mobile Country Codes (MCC) as defined in ITU E.212 for use in identifying mobile stations in wireless telephone networks, particularly GSM and UMTS networks.|
|mnc|string|Mobile Network Code (MNC) is used in combination with a Mobile Country Code (MCC) to uniquely identify a mobile phone operator or carrier.|
|mobile_brand|string|Commercial brand associated with the mobile carrier.|
|elevation|integer|Average height of city above sea level in meters (m).|
|usage_type|string|Usage type classification of ISP or company.|
|address_type|string|IP address types as defined in Internet Protocol version 4 (IPv4) and Internet Protocol version 6 (IPv6).|
|continent.name|string|Continent name.|
|continent.code|string|Two-character continent code.|
|continent.hemisphere|array|The hemisphere of where the country located. The data in array format with first item indicates (north/south) hemisphere and second item indicates (east/west) hemisphere information.|
|continent.translation|object|Translation data based on the given lang code.|
|district|string|District or county name.|
|country.name|string|Country name based on ISO 3166.|
|country.alpha3_code|string|Three-character country code based on ISO 3166.|
|country.numeric_code|string|Three-character country numeric code based on ISO 3166.|
|country.demonym|string|Native of the country.|
|country.flag|string|URL of the country flag image.|
|country.capital|string|Capital of the country.|
|country.total_area|integer|Total area in km2.|
|country.population|integer|Population of the country.|
|country.currency|object|Currency of the country.|
|country.language|object|Language of the country.|
|country.tld|string|Country-Code Top-Level Domain.|
|country.translation|object|Translation data based on the given lang code.|
|region.name|string|Region or state name.|
|region.code|string|ISO3166-2 code.|
|region.translation|object|Translation data based on the given lang code.|
|city.name|string| City name.|
|city.translation|object|Translation data based on the given lang code.|
|time_zone_info.olson|string|Time zone in Olson format.|
|time_zone_info.current_time|string|Current time in ISO 8601 format.|
|time_zone_info.gmt_offset|integer|GMT offset value in seconds.|
|time_zone_info.is_dst|boolean|Indicate if the time zone value is in DST.|
|time_zone_info.sunrise|string|Time of sunrise. (hh:mm format in local time, i.e, 07:47)|
|time_zone_info.sunset|string|Time of sunset. (hh:mm format in local time, i.e 19:50)|
|geotargeting.metro|string|Metro code based on zip/postal code.|
|ads_category|string|The domain category code based on IAB Tech Lab Content Taxonomy.|
|ads_category_name|string|The domain category based on IAB Tech Lab Content Taxonomy. These categories are comprised of Tier-1 and Tier-2 (if available) level categories widely used in services like advertising, Internet security and filtering appliances.|
|is_proxy|boolean|Whether is a proxy or not.|
|proxy.last_seen|integer|Proxy last seen in days.|
|proxy.proxy_type|string|Type of proxy.|
|proxy.threat|string|Security threat reported.|
|proxy.provider|string|Name of VPN provider if available.|
|proxy.is_vpn|boolean|Anonymizing VPN services.|
|proxy.is_tor|boolean|Tor Exit Nodes.|
|proxy.is_data_center|boolean|Hosting Provider, Data Center or Content Delivery Network.|
|proxy.is_public_proxy|boolean|Public Proxies.|
|proxy.is_web_proxy|boolean|Web Proxies.|
|proxy.is_web_crawler|boolean|Search Engine Robots.|
|proxy.is_residential_proxy|boolean|Residential proxies.|
|proxy.is_spammer|boolean|Email and forum spammers.|
|proxy.is_scanner|boolean|Network security scanners.|
|proxy.is_botnet|boolean|Malware infected devices.|

```json
{
  "ip": "8.8.8.8",
  "country_code": "US",
  "country_name": "United States of America",
  "region_name": "California",
  "city_name": "Mountain View",
  "latitude": 37.405992,
  "longitude": -122.078515,
  "zip_code": "94043",
  "time_zone": "-07:00",
  "asn": "15169",
  "as": "Google LLC",
  "isp": "Google LLC",
  "domain": "google.com",
  "net_speed": "T1",
  "idd_code": "1",
  "area_code": "650",
  "weather_station_code": "USCA0746",
  "weather_station_name": "Mountain View",
  "mcc": "-",
  "mnc": "-",
  "mobile_brand": "-",
  "elevation": 32,
  "usage_type": "DCH",
  "address_type": "Anycast",
  "continent": {
    "name": "North America",
    "code": "NA",
    "hemisphere": [
      "north",
      "west"
    ],
    "translation": {
      "lang": "es",
      "value": "Norteamérica"
    }
  },
  "district": "Santa Clara County",
  "country": {
    "name": "United States of America",
    "alpha3_code": "USA",
    "numeric_code": 840,
    "demonym": "Americans",
    "flag": "https://cdn.ip2location.io/assets/img/flags/us.png",
    "capital": "Washington, D.C.",
    "total_area": 9826675,
    "population": 331002651,
    "currency": {
      "code": "USD",
      "name": "United States Dollar",
      "symbol": "$"
    },
    "language": {
      "code": "EN",
      "name": "English"
    },
    "tld": "us",
    "translation": {
      "lang": "es",
      "value": "Estados Unidos de América (los)"
    }
  },
  "region": {
    "name": "California",
    "code": "US-CA",
    "translation": {
      "lang": "es",
      "value": "California"
    }
  },
  "city": {
    "name": "Mountain View",
    "translation": {
      "lang": null,
      "value": null
    }
  },
  "time_zone_info": {
    "olson": "America/Los_Angeles",
    "current_time": "2023-09-03T18:21:13-07:00",
    "gmt_offset": -25200,
    "is_dst": true,
    "sunrise": "06:41",
    "sunset": "19:33"
  },
  "geotargeting": {
    "metro": "807"
  },
  "ads_category": "IAB19-11",
  "ads_category_name": "Data Centers",
  "is_proxy": false,
  "proxy": {
    "last_seen": 3,
    "proxy_type": "DCH",
    "threat": "-",
    "provider": "-",
    "is_vpn": false,
    "is_tor": false,
    "is_data_center": true,
    "is_public_proxy": false,
    "is_web_proxy": false,
    "is_web_crawler": false,
    "is_residential_proxy": false,
    "is_spammer": false,
    "is_scanner": false,
    "is_botnet": false
  }
}
```

### Domain WHOIS Lookup function
| Parameter | Type | Description |
|---|---|---|
|domain|string|Domain name.|
|domain_id|string|Domain name ID.|
|status|string|Domain name status.|
|create_date|string|Domain name creation date.|
|update_date|string|Domain name updated date.|
|expire_date|string|Domain name expiration date.|
|domain_age|integer|Domain name age in day(s).|
|whois_server|string|WHOIS server name.|
|registrar.iana_id|string|Registrar IANA ID.|
|registrar.name|string|Registrar name.|
|registrar.url|string|Registrar URL.|
|registrant.name|string|Registrant name.|
|registrant.organization|string|Registrant organization.|
|registrant.street_address|string|Registrant street address.|
|registrant.city|string|Registrant city.|
|registrant.region|string|Registrant region.|
|registrant.zip_code|string|Registrant ZIP Code.|
|registrant.country|string|Registrant country.|
|registrant.phone|string|Registrant phone number.|
|registrant.fax|string|Registrant fax number.|
|registrant.email|string|Registrant email address.|
|admin.name|string|Admin name.|
|admin.organization|string|Admin organization.|
|admin.street_address|string|Admin street address.|
|admin.city|string|Admin city.|
|admin.region|string|Admin region.|
|admin.zip_code|string|Admin ZIP Code.|
|admin.country|string|Admin country.|
|admin.phone|string|Admin phone number.|
|admin.fax|string|Admin fax number.|
|admin.email|string|Admin email address.|
|tech.name|string|Tech name.|
|tech.organization|string|Tech organization.|
|tech.street_address|string|Tech street address.|
|tech.city|string|Tech city.|
|tech.region|string|Tech region.|
|tech.zip_code|string|Tech ZIP Code.|
|tech.country|string|Tech country.|
|tech.phone|string|Tech phone number.|
|tech.fax|string|Tech fax number.|
|tech.email|string|Tech email address.|
|billing.name|string|Billing name.|
|billing.organization|string|Billing organization.|
|billing.street_address|string|Billing street address.|
|billing.city|string|Billing city.|
|billing.region|string|Billing region.|
|billing.zip_code|string|Billing ZIP Code.|
|billing.country|string|Billing country.|
|billing.phone|string|Billing phone number.|
|billing.fax|string|Billing fax number.|
|billing.email|string|Billing email address.|
|nameservers|array|Name servers|

```json
{
    "domain": "locaproxy.com",
    "domain_id": "1710914405_DOMAIN_COM-VRSN",
    "status": "clientTransferProhibited https://icann.org/epp#clientTransferProhibited",
    "create_date": "2012-04-03T02:34:32Z",
    "update_date": "2021-12-03T02:54:57Z",
    "expire_date": "2024-04-03T02:34:32Z",
    "domain_age": 3863,
    "whois_server": "whois.godaddy.com",
    "registrar": {
        "iana_id": "146",
        "name": "GoDaddy.com, LLC",
        "url": "https://www.godaddy.com"
    },
    "registrant": {
        "name": "Registration Private",
        "organization": "Domains By Proxy, LLC",
        "street_address": "DomainsByProxy.com",
        "city": "Tempe",
        "region": "Arizona",
        "zip_code": "85284",
        "country": "US",
        "phone": "+1.4806242599",
        "fax": "+1.4806242598",
        "email": "Select Contact Domain Holder link at https://www.godaddy.com/whois/results.aspx?domain=LOCAPROXY.COM"
    },
    "admin": {
        "name": "Registration Private",
        "organization": "Domains By Proxy, LLC",
        "street_address": "DomainsByProxy.com",
        "city": "Tempe",
        "region": "Arizona",
        "zip_code": "85284",
        "country": "US",
        "phone": "+1.4806242599",
        "fax": "+1.4806242598",
        "email": "Select Contact Domain Holder link at https://www.godaddy.com/whois/results.aspx?domain=LOCAPROXY.COM"
    },
    "tech": {
        "name": "Registration Private",
        "organization": "Domains By Proxy, LLC",
        "street_address": "DomainsByProxy.com",
        "city": "Tempe",
        "region": "Arizona",
        "zip_code": "85284",
        "country": "US",
        "phone": "+1.4806242599",
        "fax": "+1.4806242598",
        "email": "Select Contact Domain Holder link at https://www.godaddy.com/whois/results.aspx?domain=LOCAPROXY.COM"
    },
    "billing": {
        "name": "",
        "organization": "",
        "street_address": "",
        "city": "",
        "region": "",
        "zip_code": "",
        "country": "",
        "phone": "",
        "fax": "",
        "email": ""
    },
    "nameservers": ["vera.ns.cloudflare.com", "walt.ns.cloudflare.com"]
}
```


LICENCE
=====================
See the LICENSE file.
