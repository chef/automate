defmodule Notifications.Data.Store do
  alias Notifications.{Rule, Config, RuleIdentifier}
  alias Notifications.Data.TimedCache
  alias Notifications.Data.SecretStore
  require Logger

  # Public interface
  def startup() do
    # sqerl expects all of its settings in application environment,
    # so let's make sure they're in place before we start sqerl.
    sqerl = Config.sqerl_settings()
    for {k, v} <- sqerl, do:  Application.put_env(:sqerl, k, v)
    :application.ensure_all_started(:sqerl)
    pool = [{:name, :sqerl},
            {:max_count, 10},
            {:init_count, 3},
            {:start_mfa, {:sqerl_client, :start_link, []}}]
    {:ok, _pid} = :pooler.new_pool(pool)

    :ok
  end

  # This is our only high-traffic call - we hit it for every inbound
  # notification. We're still using the same underlying call (get_rules_for_event)
  # but the cache will control how often it executes against the DB.
  def get_targets_for_event(event) when is_atom(event) do
    get_targets_for_event(unqualified_type_name(event))
  end
  def get_targets_for_event(event) when is_binary(event) do
    case TimedCache.get({:targets_for_event, event}) do
      {:error, :not_cached} -> init_event_target_cache(event)
      data -> data
    end
  end

  def get_rule(%RuleIdentifier{id: id}) do
    fetch_rules(:get_rule_by_id, [id])
  end

  def get_rules() do
    fetch_rules(:get_rules, [])
  end

  def add_rule(%Rule{} = rule) do
    # TODO - make pgsql do uuid and return it from the insert; remove the uuid lib dep
    id = UUID.uuid1()

    params = mk_rule_statement_params(id, rule)
    case :sqerl.statement(:add_rule, params) do
      {:ok, 1} -> {:ok, id}
      {:conflict, _} -> {:error, :conflict}
      {:error, _} = error -> error
    end
  end

  def delete_rule(%RuleIdentifier{id: id}) do
    case delete_secret_for_rule_id(id) do
      {:ok} ->
        case :sqerl.execute(:delete_rule, [id]) do
          {:ok, 1} -> :ok
          {:ok, 0} -> :not_found
          {:error, _} = error -> error
        end
      {:error, _} = error -> error
      :not_found -> :not_found
    end
  end

  def update_rule(%Rule{} = rule) do
    params = mk_rule_statement_params(rule.id, rule)
    case :sqerl.execute(:update_rule, params) do
      {:ok, 0} -> :not_found
      {:ok, 1, _} -> :ok
      {:conlict, _} -> {:error, :conflict}
      {:error, _} = error -> error
    end
  end

  def event_processed?(type, id) when is_atom(type) do
    event_processed?(unqualified_type_name(type), id)
  end

  def event_processed?(type, id) do
    # This invokes the function 'log_and_clean_event' as defined in migrations.ex.
    # It will purge expired events, and return true if insert fails due to duplicated
    # ID+type in the remaining records.
    #
    # The table used is not captured by WAL which should keep transaction times short even
    # when the DB is under load.
    #
    # Combining these into single function saves a second round trip to the DB per inbound
    # notification.
    case Config.dedup_window_seconds() do
      0 -> false # dedup window 0 disables duplicate checking
      window ->
        case :sqerl.execute(:log_and_clean_event, [id, type, window]) do
          {:ok, [[{"log_and_clean_event",  already_processed}]]} ->
            already_processed
          {:error, _} = error -> error
        end
    end
  end

  # Callback specified in Config.sqerl_settings. Used by sqerl to
  # load the list of sql statements to be cached as prepared statements.
  def statements() do
    [
      {:ping, "SELECT 'pong' as ping LIMIT 1"}, # used by sqerl to test for aliveness
      {:add_rule, "INSERT INTO rules (id, name, event, action, url, secret_id, critical_controls_only) values ($1, $2, $3, $4, $5, $6, $7)"},
      {:delete_rule, "DELETE FROM rules WHERE id = $1"},
      {:get_rule_by_id, "SELECT id, name, event, action, url, secret_id, critical_controls_only FROM rules WHERE id = $1"},
      {:get_rules, "SELECT id, name, event, action, url, secret_id, critical_controls_only FROM rules ORDER BY id"},
      {:get_rules_for_event, "SELECT id, name, event, action, url, secret_id, critical_controls_only FROM rules WHERE event = $1 ORDER BY id"},
      # PG normally returns number of rows updated - so if we find a match but there is no
      # change to it, it will return 0 rows updated.  To work around that we declare "RETURNING id" -
      # this makes sure if the record is found, the ID is returned even if nothing changes.
      # When the record is not found, this will not be included in the response.
      {:update_rule, "UPDATE rules SET name = $2, event = $3, action = $4, url = $5, secret_id = $6, critical_controls_only = $7 WHERE id = $1 RETURNING id"},
      {:log_and_clean_event, "SELECT log_and_clean_event($1, $2, $3)"}
    ]
  end

  # Internal Helpers

  defp delete_secret_for_rule_id(rule_id) do
    case get_secret_id(rule_id) do
      {:ok, secret_id} ->
        case secret_id do
          "" -> {:ok}
          _ -> SecretStore.delete_secret(secret_id)
        end
      :not_found -> :not_found
      {:error, _} = error -> error
    end
  end

  defp get_secret_id(rule_id) do
    case fetch_db_rules(:get_rule_by_id, [rule_id]) do
      {:ok, [db_rule]} -> 
        case propval(db_rule, "secret_id") do 
          :null -> {:ok, ""} # migrated row
          secret_id -> {:ok, secret_id}
        end
      {:ok, []} -> :not_found
      {:error, _} = error -> error
    end
  end

  defp fetch_rules(query, params) do
    case fetch_db_rules(query, params) do
      {:ok, db_rules} when is_list(db_rules) ->
        {:ok, (for db_rule <- db_rules, do: db_rule_to_rule(db_rule))}
      {:error, any} -> {:error, any}
    end
  end

  defp fetch_db_rules(query, params) do
    case :sqerl.execute(query, params) do
      {:ok, :none} -> {:ok, []}
      {:ok, rows} when is_list(rows) ->
        {:ok, rows}
      {:error, any} -> {:error, any}
    end
  end

  defp fetch_targets(query, params) do
    case fetch_rules(query, params) do
      {:ok, rules} when is_list(rules) -> 
        {:ok, (for rule <- rules, do: Notifications.TargetBuilder.create_target(rule.action, rule.event))}
      {:error, any} -> {:error, any}
    end
  end

  defp db_rule_to_rule(db_rule) do
    action_type = propval(db_rule, "action")
    url = propval(db_rule, "url")
    secret_id = propval(db_rule, "secret_id")
    critical_controls_only = propval(db_rule, "critical_controls_only")

    action = Module.concat(Notifications, action_type).new(url: url, secret_id: secret_id, critical_controls_only: critical_controls_only)
    event = rule_event_to_value(propval(db_rule, "event"))
    %Rule{id: propval(db_rule, "id"),
          name: propval(db_rule, "name"),
          event: event,
          action: {String.to_atom(action_type), action}
    }
  end

  # Initializes the cache and returns data loaded during init
  defp init_event_target_cache(event) do
    {:ok, data} = TimedCache.start_cache({:targets_for_event, event}, &fetch_targets/2,
                                         [:get_rules_for_event, [event]],
                                         Config.event_rule_cache_timeout_ms())
    {:ok, data}
  end

  defp mk_rule_statement_params(id, rule)  do
    {_, %type{url: url}} = rule.action

    secret_id = case rule.action do
      {_, %{secret_id: secret_id}} -> secret_id
      _ -> ""
    end

    critical_controls_only = case rule.action do
      {_, %type{critical_controls_only: critical_controls_only}} -> critical_controls_only
      _ -> false
    end

    [id, rule.name, rule_value_to_event(rule.event),
     unqualified_type_name(type), url, secret_id, critical_controls_only]
  end

  defp rule_event_to_value(event), do: Rule.Event.value(String.to_atom(event))
  defp rule_value_to_event(value), do: Rule.Event.key(value)
  defp propval(list, key), do: :proplists.get_value(key, list)
  defp unqualified_type_name(qualified_name), do: List.last(Module.split(qualified_name))
end
