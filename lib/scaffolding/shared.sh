# copies function named $1 to name $2
# https://stackoverflow.com/questions/1203583/how-do-i-rename-a-bash-function
copy_function() {
    declare -F $1 > /dev/null || return 1
    eval "$(echo "${2}()"; declare -f ${1} | tail -n +2)"
}
copy_function do_default_build_config __origin_do_default_build_config

insertAfter(){
   local file="$1" line="$2" newText="$3"

   sed -i "$line a $newText" $file
}

do_default_build_config() {
    #if [ -f "$PLAN_CONTEXT/hooks/init" ]; then
    #    echo "YOUR PACKAGE MAY NOT CONTAIN hooks/init"
    #    return 1
    #fi

    __origin_do_default_build_config

    if [ -d "$PLAN_CONTEXT/templates" ]; then
        build_line "Copying Templates"
        cp -r "$PLAN_CONTEXT/templates" "${pkg_prefix}/templates"
    fi

    if [ ${#automate_scaffolding_include_templates[@]} -ne 0 ]; then
        mkdir -p "${pkg_prefix}/templates"

        for template in "${automate_scaffolding_include_templates[@]}"
        do
            local template_path="$(pkg_path_for "$pkg_scaffolding")/lib/templates/$template"
            build_line "Adding template $template_path"
            cp "$template_path" "${pkg_prefix}/templates/"
        done
    fi

    build_line "Adding Platform Environment"
    mkdir -p "${pkg_prefix}/config/"
    mkdir -p "${pkg_prefix}/libexec/shell"
    cp -r "$(pkg_path_for "$pkg_scaffolding")/lib/support/"* "${pkg_prefix}/libexec/shell/"
    cat << EOF > "${pkg_prefix}/config/_a2_platform_environment"
export A2_SVC_NAME="{{pkg.name}}"
export A2_SVC_PATH="{{pkg.svc_path}}"

. ${pkg_prefix}/libexec/shell/*.sh

addNoProxy "{{sys.ip}"

chmod 0600 {{pkg.svc_config_path}}/service.crt
chmod 0600 {{pkg.svc_config_path}}/service.key
chmod 0600 {{pkg.svc_config_path}}/root_ca.crt
EOF

    if [ ! -e "${pkg_prefix}/hooks/reconfigure" ]; then
        build_line "Adding Reconfigure Hook"
        cat << EOF > "${pkg_prefix}/hooks/reconfigure"
#!{{pkgPathFor "core/bash"}}/bin/bash

chmod 0600 {{pkg.svc_config_path}}/service.crt
chmod 0600 {{pkg.svc_config_path}}/service.key
chmod 0600 {{pkg.svc_config_path}}/root_ca.crt
EOF
    fi

    build_line "Injecting Platform Environment Into Run Hook"
    insertAfter "${pkg_prefix}/hooks/run" 1 "source {{pkg.svc_config_path}}/_a2_platform_environment"

    build_line "Adding Platform JSON"
    cat << EOF > "${pkg_prefix}/config/_a2_platform.json"
{
"package": {
"ident": "{{pkg.ident}}",
"path": "{{pkg.path}}"
},
"service": {
"name": "{{pkg.name}}",
"path": "{{pkg.svc_path}}",
"tls": {
"cert_path": "{{pkg.svc_config_path}}/service.crt",
"key_path": "{{pkg.svc_config_path}}/service.key",
"root_ca_path": "{{pkg.svc_config_path}}/root_ca.crt"
}
},
{{~#if cfg._a2_platform}}
"platform":
{{ toJson cfg._a2_platform }},
{{~/if}}
"postgresql": {
{{~#eachAlive bind.automate-pg-gateway.members as |postgresql|}}
  {{~#if @last}}
"ip": "{{postgresql.sys.ip}}",
{{~#if postgresql.cfg }}
"cfg": {{ toJson postgresql.cfg }},
{{~/if}}
"__placeholder": {}
},
  {{~/if}}
{{~/eachAlive}}
"pg_sidecar": {
{{~#eachAlive bind.pg-sidecar-service.members as |pgs|}}
  {{~#if @last}}
"ip": "{{pgs.sys.ip}}",
{{~#if pgs.cfg }}
"cfg": {{ toJson pgs.cfg }},
{{~/if}}
"__placeholder": {}
{{~/if}}
{{~/eachAlive}}
},
"__placeholder": {}
}
EOF

cat << EOF > "${pkg_prefix}/config/_a2_platform_external_pg_root_ca.crt"
{{~ cfg._a2_platform.external_postgresql.ssl.root_cert ~}}
EOF

    cat << EOF > "${pkg_prefix}/config/root_ca.crt"
{{~ cfg.tls.root_cert_contents ~}}
EOF
    cat << EOF > "${pkg_prefix}/config/service.crt"
{{~ cfg.tls.cert_contents ~}}
EOF
    cat << EOF > "${pkg_prefix}/config/service.key"
{{~ cfg.tls.key_contents ~}}
EOF

}
