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
			io:format("fraud_score: ~p~n", [maps:get(<<"fraud_score">>, Result)]),
			
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
	end,
	
	Domain = "locaproxy.com",
	Result2 = ip2locationio:lookupdomainwhois(Domain),
	case Result2 of
		{error, Reason2} ->
			io:format("Error: ~p~n", [Reason2]);
		_ ->
			io:format("domain: ~p~n", [maps:get(<<"domain">>, Result2)]),
			io:format("domain_id: ~p~n", [maps:get(<<"domain_id">>, Result2)]),
			io:format("status: ~p~n", [maps:get(<<"status">>, Result2)]),
			io:format("create_date: ~p~n", [maps:get(<<"create_date">>, Result2)]),
			io:format("update_date: ~p~n", [maps:get(<<"update_date">>, Result2)]),
			io:format("expire_date: ~p~n", [maps:get(<<"expire_date">>, Result2)]),
			io:format("domain_age: ~p~n", [maps:get(<<"domain_age">>, Result2)]),
			io:format("whois_server: ~p~n", [maps:get(<<"whois_server">>, Result2)]),
			io:format("nameservers: ~p~n", [maps:get(<<"nameservers">>, Result2)]),
			
			Registrar = maps:get(<<"registrar">>, Result2),
			io:format("registrar => iana_id: ~p~n", [maps:get(<<"iana_id">>, Registrar)]),
			io:format("registrar => name: ~p~n", [maps:get(<<"name">>, Registrar)]),
			io:format("registrar => url: ~p~n", [maps:get(<<"url">>, Registrar)]),
			
			Registrant = maps:get(<<"registrant">>, Result2),
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
			
			Admin = maps:get(<<"admin">>, Result2),
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
			
			Tech = maps:get(<<"tech">>, Result2),
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
			
			Billing = maps:get(<<"billing">>, Result2),
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
	end,
	
	Punycode = ip2locationio:getpunycode("täst.de"),
	io:format("Punycode: ~p~n", [Punycode]),
	
	Normaltext = ip2locationio:getnormaltext("xn--tst-qla.de"),
	io:format("Normaltext: ~p~n", [Normaltext]),
	
	Domainname = ip2locationio:getdomainname("https://www.example.com/exe"),
	io:format("Domainname: ~p~n", [Domainname]),
	
	Domainextension = ip2locationio:getdomainextension("example.com"),
	io:format("Domainextension: ~p~n", [Domainextension]),
	
	IP2 = "8.8.8.8",
	
	Result3 = ip2locationio:lookuphosteddomain(IP2),
	case Result3 of
		{error, Reason3} ->
			io:format("Error: ~p~n", [Reason3]);
		_ ->
			io:format("ip: ~p~n", [maps:get(<<"ip">>, Result3)]),
			io:format("total_domains: ~p~n", [maps:get(<<"total_domains">>, Result3)]),
			io:format("page: ~p~n", [maps:get(<<"page">>, Result3)]),
			io:format("per_page: ~p~n", [maps:get(<<"per_page">>, Result3)]),
			io:format("total_pages: ~p~n", [maps:get(<<"total_pages">>, Result3)]),
			io:format("domains: ~p~n", [maps:get(<<"domains">>, Result3)])
	end.

