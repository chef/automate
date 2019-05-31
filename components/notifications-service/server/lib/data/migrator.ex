require Logger

defmodule Notifications.Data.Migrator do
  @moduledoc """
  This applies any migrations provided by Data.Migrations.all that have not been applied
  in order, with transactional context. While the changes are not written to be idempotent,
  the transaction around each migration ensures that we can't be left in a partially-applied
  state for a given migration.

  This module tracks which migrations have been applied via the migrations table,
  which is itself created as the first migration.

  When a migration is applied successfully, the migrations table record for that migration
  is inserted as part of the same transaction used for the migration.

  Error handling is 100% "let it crash" - since this is not user facing and the
  types of failures we might encounter (db not available, someoone has a table locked, etc) are
  transient by nature, we're relying on restarts by the service supervisor (hab, not erlang) after
  the notifications service crashes.

  Error logging will capture failure reasons as 'badmatch' errors
  """

  alias Notifications.Data.Migrator.{DB, Migrations}

  def run() do
    {:ok, conn} = DB.connect()
    current = DB.current_migration_level(conn)
    target = length(Migrations.all)
    case next_migration(current, target) do
      {:error, :notifications_service_upgrade_required} = error ->
        Logger.error("Notifications service is out of sync.  Data is migrated to #{current} but expected max of #{target}) ")
        DB.disconnect(conn)
        error
      next ->
        Logger.info("Current migration level is #{current}, next is #{next}, targeting #{target}")
        migrate(conn, next)
        DB.disconnect(conn)
      :ok
    end
  end

  def migrate(_conn, {:error, _reason} = error), do: error
  def migrate(_conn, :done), do: :ok
  def migrate(conn, migration_number) do
    migrations = Migrations.all()
    target = length(migrations)
    {detail, _} = List.pop_at(migrations, migration_number - 1)
    Logger.info("Starting migration #{migration_number}: #{detail.description}")

    # If there is only one query then do not run it in a transaction
    # This was added because an postgres ALTER TYPE can not be ran in a transaction.
    case length(detail.queries) do
      1 ->
        [query] = detail.queries
        DB.exec_migration_sql(conn, query)
        DB.add_migration_record(conn, migration_number, detail.description)
      _ ->
        DB.begin_tx(conn)
        for query <- detail.queries, do: [DB.exec_migration_sql(conn, query)]
        DB.add_migration_record(conn, migration_number, detail.description)
        DB.commit_tx(conn)
    end

    Logger.info("Finished migration #{migration_number}")
    migrate(conn, next_migration(migration_number, target))
  end

  def next_migration(last_applied, target) when last_applied > target, do: {:error, :notifications_service_upgrade_required}
  def next_migration(last_applied, target) when last_applied == target, do: :done
  def next_migration(last_applied, _target), do: last_applied + 1
end

defmodule Notifications.Data.Migrator.DB do
  @moduledoc "Database interactions required by DataMigrator."

  def connect() do
    cfg = Notifications.Config.sqerl_settings()
    options = [database: Keyword.get(cfg, :db_name), port: Keyword.get(cfg, :db_port)] ++ Keyword.get(cfg, :db_options)
    :epgsql.connect(Keyword.get(cfg, :db_host), Keyword.get(cfg, :db_user),
                    Keyword.get(cfg, :db_pass), options)
  end

  def disconnect(conn) do
    :epgsql.close(conn)
  end

  def current_migration_level(conn) do
    case :epgsql.squery(conn, "SELECT max(num) as level from migrations") do
      # relation does not exist
      {:error, {:error, :error, "42P01", _, _}} ->
        0
      # exists, but has no migrations
      {:ok, _, [{:null}]} ->
        # The table exists but has  no records - should not happen
        0
      {:ok, _, [{level}]} ->
        {real_level, _} = Integer.parse(level)
        real_level
    end
  end

  def add_migration_record(conn, level, description) do
    sql = "INSERT INTO migrations(num, descr, at) VALUES ($1, $2, CURRENT_TIMESTAMP)"
    {:ok, _} = :epgsql.equery(conn, sql, [level, description])
    :ok
  end

  def begin_tx(conn) do
    {:ok, [], []} = :epgsql.squery(conn, "BEGIN ISOLATION LEVEL SERIALIZABLE")
    :ok
  end

  def commit_tx(conn) do
    {:ok, [], []} = :epgsql.squery(conn, "COMMIT")
    :ok
  end

  def exec_migration_sql(conn, query) do
    Logger.debug fn() -> "migration query #{query}" end
    case :epgsql.equery(conn, query, []) do
      {:ok, _, _} -> :ok
      {:ok, _} -> :ok
    end
  end
end
