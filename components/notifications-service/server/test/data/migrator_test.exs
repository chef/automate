defmodule Notifications.Data.Migrator.Test do
  use ExUnit.Case
  alias Notifications.Data.Migrator
  alias Notifications.Data.Migrator.DB
  alias Notifications.Data.Migrator.Migrations

  import Mock
  def db_mock(current_level) do
      {DB,
        [], [connect: fn() -> {:ok, :connection} end,
             disconnect: fn(_) -> :ok end,
             current_migration_level: fn(_) -> current_level end,
             add_migration_record: fn(_, _, _) -> :ok end,
             begin_tx: fn(_) -> :ok end,
             commit_tx: fn(_) -> :ok end,
             exec_migration_sql: fn(_, _) -> :ok end
            ]
      }
  end

  def migrations_mock() do
      {Migrations,
        [], [all: fn() -> [ %{description: "test 1", queries: ["query1"]},
                            %{description: "test 2", queries: ["querya", "queryb"]}
                          ]
                  end ]
      }
  end

  describe "next_migration/2" do
    test "returns the next migration number if the last applied migration is less than the target migration" do
      assert Migrator.next_migration(1, 2)  == 2
      assert Migrator.next_migration(4, 15)  == 5
      assert Migrator.next_migration(13, 14)  == 14
    end

    test "returns done if the last applied migration is the target migration" do
      assert Migrator.next_migration(1, 1)  == :done
      assert Migrator.next_migration(100, 100)  == :done
    end
    test "returns an error if the last applied migration is greater than the target migration" do
      assert Migrator.next_migration(2, 1) == {:error, :notifications_service_upgrade_required}
    end
  end

  describe "migrate/2" do
    test "runs all migration queries including tracking in a transaction" do
      with_mocks([migrations_mock(), db_mock(0)]) do
        Migrator.migrate(:any, 2)
        assert called DB.begin_tx(:any)
        assert called DB.exec_migration_sql(:any, "querya")
        assert called DB.exec_migration_sql(:any, "queryb")
        assert called DB.add_migration_record(:any, 2, "test 2")
        assert called DB.commit_tx(:any)
      end
    end
  end

  describe "run/0" do
    test "processes all available migrations" do
      with_mocks([migrations_mock(), db_mock(0)]) do
        assert :ok == Migrator.run()
        assert called Migrations.all()
        refute called DB.add_migration_record(:connection, 1, "test1")
        refute called DB.add_migration_record(:connection, 2, "test2")
      end
    end

    test "does nothing when there are no more migrations to run" do
      with_mocks([migrations_mock(), db_mock(2) ]) do
        assert :ok == Migrator.run()
        assert called DB.current_migration_level(:connection)
        refute called DB.add_migration_record(:_, :_, :_)
      end
    end

    test "Fails with an error when current migration level is greater than target" do
      with_mocks([migrations_mock(), db_mock(9) ]) do
        assert {:error, :notifications_service_upgrade_required} == Migrator.run()
      end
    end
  end
end
