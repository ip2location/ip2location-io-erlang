# IP2Location.io Erlang SDK — IP Geolocation, Domain WHOIS & Hosted Domain Lookup

[![GitHub Repository](https://img.shields.io/badge/GitHub-ip2location--io--erlang-181717?logo=github)](https://github.com/ip2location/ip2location-io-erlang)
[![Documentation](https://img.shields.io/badge/Docs-Read%20the%20Docs-blue)](https://ip2location-io-erlang.readthedocs.io/en/latest/index.html)

**IP2Location.io Erlang SDK** is an Erlang module for retrieving **IPv4 and IPv6 geolocation, network, and IP intelligence information** from the [IP2Location.io API](https://www.ip2location.io/). It also supports **domain WHOIS lookup**, **domain/Punycode utilities**, and **hosted-domain lookup**. The module returns decoded Erlang maps for successful API lookups.

**Quick links:** [Get an API key](https://www.ip2location.io/pricing) · [Developer documentation](https://ip2location-io-erlang.readthedocs.io/en/latest/index.html) · [API reference](https://ip2location-io-erlang.readthedocs.io/en/latest/code.html) · [Official GitHub repository](https://github.com/ip2location/ip2location-io-erlang)

## Features

- **IPv4 and IPv6 geolocation:** Look up country, region, district, city, postal code, coordinates, and time zone.
- **Network and ASN information:** Access available ASN, AS name, ISP, associated domain, connection speed, usage type, and network attributes.
- **Time zone and localization data:** Retrieve time-zone metadata, including available DST details; request translated continent, country, region, and city information on eligible plans.
- **Proxy and security indicators:** Inspect available proxy, VPN, Tor, data-center, crawler, and fraud-related signals, depending on API plan.
- **Additional IP intelligence:** Access eligible mobile carrier details (MCC/MNC), country metadata, weather-station information, and advertising categories.
- **Domain WHOIS lookup:** Retrieve available registrar, domain registration dates, statuses, nameservers, and contact information.
- **Internationalized domain and URL utilities:** Convert domain names to/from Punycode and extract domain names or extensions.
- **Hosted domains:** List domains hosted on an IPv4 or IPv6 address, with optional pagination.

> **Plan availability:** API response fields vary by plan and IP address; optional fields may be absent or unavailable. Consult [pricing](https://www.ip2location.io/pricing) and the [Erlang API reference](https://ip2location-io-erlang.readthedocs.io/en/latest/code.html).

## SDK module and functions

The SDK exposes one Erlang module, `ip2locationio`, with the following documented functions. The optional one- and two-argument variants are confirmed by the [repository source](https://github.com/ip2location/ip2location-io-erlang/blob/main/ip2locationio.erl).

| Function | Description |
| --- | --- |
| [`ip2locationio:new(APIKey)`](https://ip2location-io-erlang.readthedocs.io/en/latest/code.html) | Configures the API key for subsequent requests. |
| `ip2locationio:lookupipgeolocation(IP)` | Retrieves geolocation and available IP intelligence data. |
| `ip2locationio:lookupipgeolocation(IP, Lang)` | Requests the lookup with an optional language code. |
| `ip2locationio:lookupdomainwhois(Domain)` | Retrieves WHOIS information for a domain. |
| `ip2locationio:getpunycode(Domain)` | Converts an internationalized domain name to Punycode. |
| `ip2locationio:getnormaltext(Domain)` | Converts Punycode into Unicode domain text. |
| `ip2locationio:getdomainname(URL)` | Extracts the domain name from a URL or hostname. |
| `ip2locationio:getdomainextension(Domain)` | Extracts the domain extension. |
| `ip2locationio:lookuphosteddomain(IP)` | Retrieves the first hosted-domain results. |
| `ip2locationio:lookuphosteddomain(IP, Page)` | Retrieves a specified results page. |

## API access and query limits

IP2Location.io provides keyless geolocation requests through its REST API and a Free plan with an API key:

| API service | Keyless (no API key) | Free plan (with API key) |
| --- | --- | --- |
| IP Geolocation API | **1,000 queries/day** | **50,000 queries/month** |
| Domain WHOIS API | — | **500 queries/month** |
| Hosted Domain API | — | **50 queries/month** |

- **Keyless access:** Limited keyless geolocation requests can be made through the [REST API](https://www.ip2location.io/ip2location-documentation); the daily allowance resets at **00:00 UTC**. The Erlang SDK documented here expects an API key.
- **Free plan:** Obtain an API key to use the applicable monthly allowances.
- **Attribution:** Keyless and Free-plan usage require attribution; refer to the [pricing page](https://www.ip2location.io/pricing) for current terms.
- **Paid plans:** Refer to the [pricing page](https://www.ip2location.io/pricing) for higher quotas and more fields.

For authentication and usage-limit details, see the [IP Geolocation API documentation](https://www.ip2location.io/ip2location-documentation).

## Requirements

- **Erlang/OTP** with the `erl` runtime and `erlc` compiler.
- An [IP2Location.io API key](https://www.ip2location.io/pricing) for SDK lookups.
- Network access to the API endpoints.
- **`jiffy` and `idna`** dependencies: the repository's `mix.exs` declares `jiffy ~> 1.1` and `idna ~> 6.1`. The source uses `jiffy` for JSON decoding and `idna` for internationalized domain conversion.
- **Elixir/Mix**, if you choose the repository's dependency-management workflow below. The official documentation also shows direct compilation with `erlc`.

> The official project does **not** state a specific minimum Erlang/OTP release in its quickstart. Check your local runtime and dependency compatibility rather than assuming a version requirement.

## Installation

### 1. Obtain the SDK source

Clone the official repository and change into its directory:

```bash
git clone https://github.com/ip2location/ip2location-io-erlang.git
cd ip2location-io-erlang
```

### 2. Install dependencies and compile

**Option A — Repository Mix workflow (includes dependencies):**

The repository contains a `mix.exs` manifest declaring `jiffy` and `idna`:

```bash
mix deps.get
mix compile
```

This workflow requires Elixir/Mix in addition to Erlang/OTP.

**Option B — Official direct Erlang compilation:**

The [Read the Docs quickstart](https://ip2location-io-erlang.readthedocs.io/en/latest/quickstart.html) shows:

```bash
erlc ip2locationio.erl
erlc test.erl
```

Direct compilation does **not** download third-party dependencies. Ensure `jiffy` and `idna` are installed and available on the Erlang code path before invoking SDK methods that use them.

### 3. Configure your API key

Keep your API key outside tracked source files. For the examples in this README, set `IP2LOCATION_API_KEY` in the terminal before starting the Erlang runtime.

**Linux / macOS:**

```bash
export IP2LOCATION_API_KEY='YOUR_API_KEY'
```

**Windows Command Prompt:**

```bat
set IP2LOCATION_API_KEY=YOUR_API_KEY
```

The official quickstart uses `APIKey = "YOUR_API_KEY"` directly; the environment-variable approach here is an **additional practice for local development**, not a special configuration mechanism in the SDK. Examples explicitly pass the key to `ip2locationio:new/1`.

## Quickstart: IP geolocation lookup

The official quickstart uses `ip2locationio:new/1`, `lookupipgeolocation/2`, and Erlang map access. This self-contained example uses the default-language `lookupipgeolocation/1` variant available in the repository source.

Create `lookup.erl`:

```erlang
-module(lookup).
-export([run/0]).

run() ->
    APIKey = os:getenv("IP2LOCATION_API_KEY"),
    ip2locationio:new(APIKey),
    Result = ip2locationio:lookupipgeolocation("8.8.8.8"),
    case Result of
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]);
        _ ->
            io:format("Country: ~p~n", [maps:get(<<"country_name">>, Result, undefined)]),
            io:format("City: ~p~n", [maps:get(<<"city_name">>, Result, undefined)]),
            io:format("ASN: ~p~n", [maps:get(<<"asn">>, Result, undefined)])
    end.
```

Compile the example:

```bash
erlc lookup.erl
```

Run it from an Erlang environment where the SDK and its dependencies are on the code path. For example, after `mix deps.get` and `mix compile`, you can start `iex -S mix` in the repository directory and call:

```erlang
:lookup.run()
```

The `:lookup.run()` invocation above is **Elixir IEx syntax** for calling the Erlang module. In a plain Erlang shell (with the required modules available), use `lookup:run().` instead.

Successful API responses are decoded as maps with binary keys such as `<<"country_name">>`. The exact fields depend on your plan and the queried IP address.

## More usage examples

The snippets below assume `ip2locationio:new(APIKey)` has already been called. They follow the official methods; additional field-extraction examples use the documented response map keys. When running each snippet independently, obtain and pass your API key first.

### Request translated location names

The optional language code in `lookupipgeolocation/2` supports translations of eligible place names on the **Plus and Security** plans.

```erlang
Result = ip2locationio:lookupipgeolocation("8.8.8.8", "es"),
io:format("~p~n", [Result]).
```

### Retrieve ASN and ISP information

**Additional example using documented geolocation response fields:**

```erlang
Result = ip2locationio:lookupipgeolocation("8.8.8.8"),
case Result of
    {error, Reason} -> io:format("Error: ~p~n", [Reason]);
    _ ->
        io:format("ASN: ~p~n", [maps:get(<<"asn">>, Result, undefined)]),
        io:format("AS name: ~p~n", [maps:get(<<"as">>, Result, undefined)]),
        io:format("ISP: ~p~n", [maps:get(<<"isp">>, Result, undefined)])
end.
```

### Inspect available proxy indicators

**Additional example using documented geolocation response fields:** proxy details may not be included in all plans or all responses.

```erlang
Result = ip2locationio:lookupipgeolocation("8.8.8.8"),
case Result of
    {error, Reason} -> io:format("Error: ~p~n", [Reason]);
    _ ->
        io:format("Is proxy: ~p~n", [maps:get(<<"is_proxy">>, Result, undefined)]),
        Proxy = maps:get(<<"proxy">>, Result, #{}),
        io:format("VPN: ~p~n", [maps:get(<<"is_vpn">>, Proxy, undefined)]),
        io:format("Tor: ~p~n", [maps:get(<<"is_tor">>, Proxy, undefined)])
end.
```

### Look up domain WHOIS information

The official quickstart documents `lookupdomainwhois/1`. WHOIS records may contain redacted or missing contact information.

```erlang
Result = ip2locationio:lookupdomainwhois("example.com"),
case Result of
    {error, Reason} -> io:format("Error: ~p~n", [Reason]);
    _ ->
        io:format("Domain: ~p~n", [maps:get(<<"domain">>, Result, undefined)]),
        io:format("Created: ~p~n", [maps:get(<<"create_date">>, Result, undefined)]),
        io:format("Expires: ~p~n", [maps:get(<<"expire_date">>, Result, undefined)]),
        io:format("Nameservers: ~p~n", [maps:get(<<"nameservers">>, Result, undefined)])
end.
```

See the [WHOIS response field reference](https://ip2location-io-erlang.readthedocs.io/en/latest/code.html) for additional values.

### Use domain and Punycode utilities

These helpers are documented in the official quickstart:

```erlang
Punycode = ip2locationio:getpunycode("täst.de"),
NormalText = ip2locationio:getnormaltext("xn--tst-qla.de"),
DomainName = ip2locationio:getdomainname("https://www.example.com/path"),
DomainExtension = ip2locationio:getdomainextension("example.com"),
io:format("Punycode: ~p~nNormal text: ~p~nDomain: ~p~nExtension: ~p~n",
          [Punycode, NormalText, DomainName, DomainExtension]).
```

The Punycode helpers rely on the `idna` dependency.

### Find domains hosted on an IP address

The official quickstart shows `lookuphosteddomain/1`. The repository source additionally exports `lookuphosteddomain/2` for pagination.

```erlang
Result = ip2locationio:lookuphosteddomain("8.8.8.8"),
case Result of
    {error, Reason} -> io:format("Error: ~p~n", [Reason]);
    _ ->
        io:format("Total domains: ~p~n", [maps:get(<<"total_domains">>, Result, undefined)]),
        io:format("Domains: ~p~n", [maps:get(<<"domains">>, Result, [])])
end.
```

To request a later page, supply the page number:

```erlang
Page2 = ip2locationio:lookuphosteddomain("8.8.8.8", 2),
io:format("~p~n", [Page2]).
```

Hosted-domain results may include `page`, `per_page`, and `total_pages` along with `domains`.

## Erlang setup and troubleshooting

- **`undef` for `ip2locationio` or `lookup`:** Check that the module compiled and its `.beam` file is on the Erlang code path.
- **`undef` for `jiffy` or `idna`:** Install the repository's declared dependencies and start the runtime with their compiled modules available; `erlc` alone does not fetch dependencies.
- **Missing API key:** Call `ip2locationio:new(APIKey)` with a valid key before performing lookups. When using environment variables, set them before starting the runtime.
- **Missing response field:** Use `maps:get(Key, Map, Default)` for optional fields, and check the plan-specific API response.
- **Compilation versus execution:** The official Read the Docs page documents direct compilation, while the repository's `mix.exs` provides a separate dependency workflow. The execution examples here explain the distinction.

## Common use cases

- **Website and service localization:** Choose approximate regional content or language based on an IP address.
- **Traffic analytics:** Enrich server events with country, city, ISP, or ASN information.
- **Security review:** Use available proxy, VPN, Tor, and fraud-related signals as supporting context rather than sole evidence of abuse.
- **Domain research:** Inspect registration dates, nameservers, and other available WHOIS information.
- **Network and hosting research:** Investigate autonomous systems and domains associated with an IP address.

## Documentation and support

- [Erlang SDK quickstart and official sample codes](https://ip2location-io-erlang.readthedocs.io/en/latest/quickstart.html)
- [Erlang SDK API reference](https://ip2location-io-erlang.readthedocs.io/en/latest/code.html)
- [Official GitHub repository](https://github.com/ip2location/ip2location-io-erlang)
- [Official SDK implementation](https://github.com/ip2location/ip2location-io-erlang/blob/main/ip2locationio.erl)
- [Repository test examples](https://github.com/ip2location/ip2location-io-erlang/blob/main/test.erl)
- [Dependency manifest (`mix.exs`)](https://github.com/ip2location/ip2location-io-erlang/blob/main/mix.exs)
- [IP2Location.io API plans and query limits](https://www.ip2location.io/pricing)
- [IP Geolocation API documentation](https://www.ip2location.io/ip2location-documentation)
- [Report an issue or request a feature](https://github.com/ip2location/ip2location-io-erlang/issues)
- [API and account support](https://www.ip2location.io/contact-us)

## License

Licensed under the **MIT License**. See [LICENSE](./LICENSE).
