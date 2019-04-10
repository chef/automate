export BUILDER_KEY_PATH="{{pkg.svc_var_path}}/{{cfg.ssh_git.builder_key}}"
export ESCRIPT_PATH="{{pkg.path}}/delivery/bin"
export ERL_COOKIE=$(cat "{{pkg.svc_var_path}}/.erlang.cookie")
export DELIVERY_RB="{{pkg.svc_config_path}}/delivery.rb"
export ENTERPRISE_CTL_COMMAND="{{pkg.path}}/delivery/bin/enterprise_ctl"
export PATH=$(cat {{pkg.path}}/RUNTIME_PATH):/bin
