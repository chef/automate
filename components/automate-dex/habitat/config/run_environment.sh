#!{{pkgPathFor "core/bash"}}/bin/bash
{{~#if cfg.connectors.ldap}}
export LDAP_BINDDN=$(sed 's/^\"\(.*\)\"$/\1/' {{pkg.svc_config_path}}/ldap_bind_dn)
export LDAP_BINDPW=$(secrets-helper show userconfig.ldap_password || echo "")
{{~/if}}
{{~#if cfg.connectors.msad_ldap}}
export LDAP_BINDDN=$(sed 's/^\"\(.*\)\"$/\1/' {{pkg.svc_config_path}}/ldap_bind_dn)
export LDAP_BINDPW=$(secrets-helper show userconfig.msad_password || echo "")
{{~/if}}
