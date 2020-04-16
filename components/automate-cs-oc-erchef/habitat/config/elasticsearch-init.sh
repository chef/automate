#!{{pkgPathFor "core/bash"}}/bin/bash -e

{{#eachAlive bind.automate-es-gateway.members as |member|}}
  {{#if @last}}
HOST="127.0.0.1"
PORT="{{member.cfg.http-port}}"
  {{/if}}
{{/eachAlive}}

init-es $HOST:$PORT "chef" "{{pkg.svc_config_path}}/elasticsearch-index-init.json"
