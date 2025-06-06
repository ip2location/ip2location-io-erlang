defmodule IP2LocationIOErlang.MixProject do
  use Mix.Project

  def project() do
    [
      app: :ip2location_io_erlang,
      version: "1.1.1",
      elixir: "~> 1.0",
      erlc_paths: ["."],
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "ip2location_io_erlang",
      source_url: "https://github.com/ip2location/ip2location-io-erlang"
    ]
  end

  def application() do
    []
  end

  defp deps() do
    [
      {:jiffy, "~> 1.1"},
      {:idna, "~> 6.1"},
      {:ex_doc, "~> 0.14", only: :dev, runtime: false}
    ]
  end

  defp description() do
    "Query country, region, city, latitude, longitude, ZIP code, time zone, ISP, domain, connection type, IDD, area code, weather station, mcc, mnc, mobile brand, elevation, usage type, address type, IAB category, fraud score, district, (ASN) and (AS) from IP address by using IP2Location.io API."
  end

  defp package() do
    [
      # This option is only needed when you don't want to use the OTP application name
      name: "ip2location_io_erlang",
      # These are the default files included in the package
      files: ~w(mix.exs README* LICENSE* *.erl),
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/ip2location/ip2location-io-erlang"}
    ]
  end
end