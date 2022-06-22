# we do not need a shebang
# shellcheck disable=SC2148

# we do not need to be warned about handlebars template variables
# shellcheck disable=SC1083

if ! which psql >/dev/null; then
  # UPSTREAM_PKG_IDENT is replaced at pkg build time
  PATH=$PATH:$(HAB_LICENSE=accept-no-persist hab pkg path "UPSTREAM_PKG_IDENT")/bin
  export PATH
fi

{{ #if cfg.ssl.enable }}
PGSSLMODE=require
{{ else }}
PGSSLMODE=disable
{{ /if }}
local_connect_string="-U {{cfg.superuser.name}} -h {{sys.ip}} -p {{cfg.port}}"
primary_connect_string="-U {{cfg.superuser.name}} -h {{svc.leader.sys.ip}} -p {{svc.leader.cfg.port}}"
export primary_connect_string local_connect_string PGSSLMODE
export PGPASSFILE="{{pkg.svc_config_path}}/.pgpass"

write_local_conf() {
  echo 'Writing postgresql.local.conf file based on memory settings'
  cat > {{pkg.svc_config_path}}/postgresql.local.conf<<LOCAL
# Auto-generated memory defaults created at service start by Habitat
effective_cache_size=$(awk '/MemTotal/ {printf( "%.0f\n", ($2 / 1024 / 4) *3 )}' /proc/meminfo)MB
shared_buffers=$(awk '/MemTotal/ {printf( "%.0f\n", $2 / 1024 / 4 )}' /proc/meminfo)MB
maintenance_work_mem=$(awk '/MemTotal/ {printf( "%.0f\n", $2 / 1024 / 16 )}' /proc/meminfo)MB
work_mem=$(awk '/MemTotal/ {printf( "%.0f\n", (($2 / 1024 / 4) *3) / ({{cfg.max_connections}} *3) )}' /proc/meminfo)MB
temp_buffers=$(awk '/MemTotal/ {printf( "%.0f\n", (($2 / 1024 / 4) *3) / ({{cfg.max_connections}}*3) )}' /proc/meminfo)MB
LOCAL
}

write_env_var() {
  echo "$1" > "{{pkg.svc_config_path}}/env/$2"
}

pg_reload_config() {
  psql ${primary_connect_string} -d postgres -t -c 'SELECT pg_reload_conf()'
}

leader_uptime_seconds() {
  # return uptime in seconds of leader
  local uptime=$(psql ${primary_connect_string} -d postgres -t -c 'select FLOOR(extract(epoch from current_timestamp - pg_postmaster_start_time())) as uptime' | head -n 1 | sed -e 's/^ *//g')
  echo "${uptime}"
}

replica_last_replay_seconds() {
  # return number of seconds since last time WAL was applied
  local lag=$(psql ${local_connect_string} -d postgres -t -c "SELECT FLOOR(extract(epoch from now() - pg_last_xact_replay_timestamp())) AS lag" | head -n 1 | sed -e 's/^ *//g')
  echo "${lag}"
}

pg_is_not_in_recovery() {
  if psql ${primary_connect_string} -d postgres -t -c 'SELECT pg_is_in_recovery()' | grep -q '^ *f$'; then
    return 0 # true
  else
    return 1 # false
  fi
}

cluster_backup() {
  if pg_is_not_in_recovery; then
    echo "Executing backup.."
    pg_dumpall ${primary_connect_string} --clean --encoding=utf8 --if-exists | gzip > {{cfg.pg_dump.path}}/$(date +"%Y%m%d%H%M%S")-pgdumpall.gz
  else
    echo "This node is in recovery, not executing backup!"
  fi
}

wait_for_leader() {
  if pg_is_not_in_recovery; then
    return 0 # true
  else
    echo "Leader is not yet available"
    exit 2 # critical from health check perspective
  fi
}

setup_replication_user_in_primary() {
  wait_for_leader
  echo 'Ensuring replication role exists with correct password'
  # store state of xtrace option
  tracestate="$(shopt -po xtrace)"
  # disable xtrace so as not to leak sensitive data
  set +x
  pass=$(awk -F: '/:{{cfg.replication.name}}:/{printf $5}' "$PGPASSFILE" | sed "s,',\\\',g" | tr -d '\n')
  psql ${primary_connect_string} -d postgres << EOF
DO \$$
BEGIN
    SET synchronous_commit = off;
    CREATE ROLE {{cfg.replication.name}} WITH REPLICATION LOGIN PASSWORD '$pass';
EXCEPTION
    WHEN duplicate_object THEN
      ALTER ROLE {{cfg.replication.name}} WITH REPLICATION LOGIN PASSWORD '$pass';
END\$$;
EOF
  # restore the value of xtrace to its original value
  eval "$tracestate"
}

checkpoint() {
  echo "Forcing a transaction log checkpoint"
  psql ${primary_connect_string} -d postgres << EOF
CHECKPOINT;
EOF
}

local_xlog_position_online() {
  psql ${local_connect_string} -d postgres -t <<EOF | tr -d '[:space:]'
SELECT CASE WHEN pg_is_in_recovery()
  THEN GREATEST(pg_wal_lsn_diff(COALESCE(pg_last_wal_receive_lsn(), '0/0'), '0/0')::bigint,
                pg_wal_lsn_diff(pg_last_wal_replay_lsn(), '0/0')::bigint)
  ELSE pg_wal_lsn_diff(pg_current_wal_lsn(), '0/0')::bigint
END;
EOF
}

get_pgcontroldata_value() {
  pg_controldata --pgdata {{pkg.svc_data_path}}/pgdata | \
    grep "${1}" | \
    awk -F: '{gsub(/^[ \t]+|[ \t]+$/, "", $2); print $2}'
}

# Use pg_controldata to putput our current xlog position, in the case that
# PG is offline - for example if starting up after a full cluster shutdown
# and needing to elect a leader
# Special thanks to Patroni for figuring this out: https://github.com/zalando/patroni/blob/master/patroni/postgresql.py#L1272-L1286
local_xlog_position_offline() {
  lsn_hex=$(get_pgcontroldata_value 'Minimum recovery ending location')
  timeline=$(get_pgcontroldata_value "Min recovery ending loc's timeline")

  if [[ $lsn_hex == '0/0' || $timeline == '0' ]]
  then
    # it was a leader when it crashed, use a different attribute
    lsn_hex=$(get_pgcontroldata_value 'Latest checkpoint location')
    timeline=$(get_pgcontroldata_value "Latest checkpoint's TimeLineID")
  fi

  # This perl one-liner returns 0 if lsn_hex is empty, in case either pg_controldata or grep failed
  # otherwise it converts the hex log position to a decimal
  # Borrowed from patroni and converted to Perl, since we already dep on a Perl interpeter
  perl -le 'my $lsn = $ARGV[0]; my @t = split /\//, $lsn; my $lsn_dec = hex(@t[0]) * hex(0x100000000) + hex(@t[1]); print $lsn_dec' -- "${lsn_hex}"
}

local_xlog_position() {
  if pg_isready ${local_connect_string} -d postgres >/dev/null
  then
    local_xlog_position_online
  else
    local_xlog_position_offline
  fi
}

primary_xlog_position() {
  psql ${primary_connect_string} -d postgres -t <<EOF | tr -d '[:space:]'
SELECT pg_wal_lsn_diff(pg_current_wal_lsn(), '0/0')::bigint;
EOF
}

primary_ready() {
  pg_isready ${primary_connect_string} --dbname=postgres --timeout=10 >/dev/null
}

ensure_dir_ownership() {
  paths="{{pkg.svc_var_path}} {{pkg.svc_data_path}}/pgdata {{cfg.wal_archive.path}} {{cfg.pg_dump.path}}"
  if [[ $EUID -eq 0 ]]; then
    # if EUID is root, so we should chown to pkg_svc_user:pkg_svc_group
    ownership_command="chown -RL {{pkg.svc_user}}:{{pkg.svc_group}}"
  else
    # not root, so at best we can only chgrp to the effective user's primary group.
    ownership_command="chgrp -RL $(id -g)"
  fi
  echo "Ensuring proper ownership: $ownership_command"
  # Noop unless the group needs changing. This keeps the read-only side of a DR setup happy.
  for path in $paths; do
    if [[ $(stat -c "%g" $path) -ne "$(id -g)" ]]; then
      eval $ownership_command $path
    fi
  done
  chmod 0700 {{pkg.svc_data_path}}/pgdata
  chmod 0600 {{pkg.svc_config_path}}/.pgpass
  server_key={{pkg.svc_config_path}}/server.key
  if [ -f "$server_key" ]; then
    chmod 0600 "$server_key"
  fi
}

promotion_trigger_file() {
  echo "promote_to_leader_trigger"
}

promote_to_leader() {
  echo "Promoting database"
  touch "{{pkg.svc_var_path}}/$(promotion_trigger_file)"
}

remove_promotion_trigger() {
  echo "Removing promotion trigger file"
  rm -f "{{pkg.svc_var_path}}/$(promotion_trigger_file)" > /dev/null
}

print_statistics() {
  echo -n "Databases and sizes:"
  psql ${local_connect_string} -d postgres -t -c "SELECT json_agg(t) FROM (SELECT * FROM pg_database WHERE datname NOT ILIKE 'template%') t"
  echo -n "Database statistics:"
  psql ${local_connect_string} -d postgres -t -c "SELECT json_agg(t) FROM (SELECT * FROM pg_stat_database WHERE datname NOT ILIKE 'template%') t"
  echo -n "Replication statistics:"
  psql ${local_connect_string} -d postgres -t -c "SELECT json_agg(t) FROM (SELECT * FROM pg_stat_replication) t"
}

check_pgdata_upgrade_needed() {
  if [[ -f "{{pkg.svc_data_path}}/PG_VERSION" ]]; then
    runtime_version=$(postgres --version | cut -d' ' -f 3)
    # Trim the micro-version for understanding upgrade boundaries
    #  (ex: 11.1 -> 11,  9.6.8 -> 9.6)
    runtime_version_nomicro="${runtime_version%.*}"
    pgdata_version="$(cat {{pkg.svc_data_path}}/pgdata/PG_VERSION)"

    if [[ "${runtime_version_nomicro}" != "${pgdata_version}" ]]
    then
      echo "ERROR: the database version on disk (${pgdata_version}) needs to be upgraded"
      echo "before it can be used with this version of PostgreSQL (${runtime_version})."
      echo "this package doesn't yet know how to do that, so pg_upgrade must be run"
      echo "manually before continuing"
      exit 1
    fi
  fi
}
