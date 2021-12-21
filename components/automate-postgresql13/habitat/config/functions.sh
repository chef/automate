write_local_conf() {
  echo 'Writing postgresql.local.conf file based on memory settings'
  cat > {{pkg.svc_config_path}}/postgresql.local.conf<<LOCAL
# Auto-generated memory defaults created at service start by Habitat
{{~#if cfg.pg.effective_cache_size }}
effective_cache_size={{cfg.pg.effective_cache_size}}
{{else}}
effective_cache_size=$(awk '/MemTotal/ {printf( "%.0f\n", ($2 / 1024 / 4) *3 )}' /proc/meminfo)MB
{{~/if}}
{{~#if cfg.pg.shared_buffers }}
shared_buffers={{cfg.pg.shared_buffers}}
{{else}}
shared_buffers=$(awk '/MemTotal/ {printf( "%.0f\n", $2 / 1024 / 4 )}' /proc/meminfo)MB
{{~/if}}
{{~#if cfg.pg.work_mem }}
work_mem={{cfg.pg.work_mem}}
{{else}}
work_mem=$(awk '/MemTotal/ {printf( "%.0f\n", (($2 / 1024 / 4) *3) / ({{cfg.pg.max_connections}} *3) )}' /proc/meminfo)MB
{{~/if}}
maintenance_work_mem=$(awk '/MemTotal/ {printf( "%.0f\n", $2 / 1024 / 16 )}' /proc/meminfo)MB
temp_buffers=$(awk '/MemTotal/ {printf( "%.0f\n", (($2 / 1024 / 4) *3) / ({{cfg.pg.max_connections}}*3) )}' /proc/meminfo)MB
LOCAL
}

ensure_dir_ownership() {
  paths="{{pkg.svc_var_path}} {{pkg.svc_data_path}}"
  if [[ $EUID -eq 0 ]]; then
    # if EUID is root, so we should chown to pkg_svc_user:pkg_svc_group
    ownership_command="chown -RL {{pkg.svc_user}}:{{pkg.svc_group}} $paths"
  else
    # not root, so at best we can only chgrp to the effective user's primary group
    ownership_command="chgrp -RL $(id -g) $paths"
  fi
  echo "Ensuring proper ownership: $ownership_command"
  $ownership_command
  chmod 0700 {{pkg.svc_data_path}}/pgdata
}

ensure_key_ownership() {
  keys=(server.crt server.key root.crt)
  for k in "${keys[@]}"
  do
    chmod 0600 "{{pkg.svc_config_path}}/$k"
  done
}
