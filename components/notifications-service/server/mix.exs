defmodule Notifications.Mixfile do
  use Mix.Project

  def project do
    # When we with habitat first, it makes it impossible to build
    # again on the host without habitat, because files outbput to _build
    # under hab are owned by root.
    #
    # To avoid this, and to ensure no carry-over from local dev builds, we'll
    # give habitat a fully build output location.
    build_path = case Mix.env do
      :habitat -> "_habitat_build"
      _ -> "_build"
    end

    [app: :notifications,
     version: String.trim(File.read!("../VERSION")),
     config_path: "config.exs",
     build_path: build_path,
     deps_path: "#{build_path}/deps",
     lockfile: "mix.lock",
     elixir: "~> 1.5",
     build_embedded: Mix.env == :habitat,
     start_permanent: Mix.env == :habitat,
     deps: deps()]
  end

  def application do
    [applications: [:grpc],
     included_applications: [
       :poison,
       :sqerl,
       :pooler,
       :envy],
     extra_applications: [
       :logger,
       :timex,
       :httpoison,
       :uuid
     ],
     mod: {Notifications.Application, []}]
  end

  defp deps do
    [
      # GRPC endpoint: service to service, currently
      #                used by ingest service to trigger
      #                notifications
      {:protobuf, github: "tony612/protobuf-elixir", override: true},
      {:grpc, github: "elixir-grpc/grpc"},
      # database w/ pooling for pgsql access
      {:sqerl, github: "chef/sqerl"},
      {:pooler, github: "seth/pooler",  erlopts: [{:d, :namespaced_types}, :debug_info, :inline], override: true},
      # Used in utils for time formatting.
      {:timex, ">= 3.1.24"},
      # JSON conversion and posting over http (outbound webhooks)
      {:poison, ">= 2.0.0"},
      {:httpoison, ">= 0.12.0"},
      {:mock, ">= 0.0.0", only: :test},
      {:dialyxir, ">= 0.0.0", only: [:dev], runtime: false},
      {:credo,  ">= 0.0.0", only: [:dev, :test], runtime: false},
      # TODO mp 2017-09-07: this means as we start to dev in habitat env, it will not
      # currently be available (there's no difference between habitat-dev and
      # habitat-prod)
      #{:exsync, ">= 0.1.0", only: :dev},
      {:exsync, github: "marcparadise/exsync", branch: "optional-source-monitoring", only: :dev},
      {:dbg, ">= 0.0.0", only: [:dev, :test]},
      {:distillery, ">= 1.0.0", []},
      {:uuid, ">= 1.1.0"},
      {:temp, "~> 0.4", only: [:test]}
    ]
  end
end
