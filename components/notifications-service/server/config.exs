use Mix.Config

# Note that notifications run-time application configuration is defined in
# Notifications.Config.  All configuration is managed via environment variaables; a
# list of supported variables can be found in notifications/config.ex.
#
# This config.exs file is used by the elixir/mix framework and provides
# status configuration for third party components/libs.

# This is the only bit of environment-specific configuration we have,
# so it's a bit cleaner to keep it in the One True Config.exs instead of
# splitting out a per-env file
log_level = case Mix.env() do
  :habitat -> :info
  _ -> :debug
end

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id, :module],
  handle_sasl_reports: true,
  handle_otp_reports: true,
  utc_log: true,
  level: log_level

config :grpc, start_server: true

# Stop lager redirecting :error_logger messages
config :lager, :error_logger_redirect, false

# Stop lager removing Logger's :error_logger handler
config :lager, :error_logger_whitelist, [Logger.ErrorHandler]

# Stop lager writing a crash log
config :lager, :crash_log, false

config :lager, :console,
  log_root: :undefined,
  handlers: [
    lager_console_backend: log_level,
    lager_file_backend: :undefined,
    lager_file_backend: :undefined
  ]

# Prevent automatic updating of timzeone data
# by the tzdata library - fetching files used at runtime
# from the internet in a production backend service is something
# we prefer to avoid...
config :tzdata, :autoupdate, :disabled
