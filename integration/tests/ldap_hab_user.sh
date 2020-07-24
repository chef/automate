#!/bin/bash

#shellcheck disable=SC2034
test_name="ldap_hab_user"
test_backup_restore=true

do_setup() {
    do_setup_default

    previous_umask=$(umask)
    umask 022
    yum -y install openldap compat-openldap openldap-clients openldap-servers nslcd nss-pam-ldapd authconfig
    systemctl start slapd

    ldapmodify -Y EXTERNAL  -H 'ldapi:///' <<EOF
dn: olcDatabase={2}hdb,cn=config
changetype: modify
replace: olcSuffix
olcSuffix: dc=example,dc=local

dn: olcDatabase={2}hdb,cn=config
changetype: modify
replace: olcRootDN
olcRootDN: cn=ldapadm,dc=example,dc=local

dn: olcDatabase={2}hdb,cn=config
changetype: modify
replace: olcRootPW
# password is password
olcRootPW: {SSHA}OiFN/myARrDNIDNkVLmDbqnO5Oh2hlR8
EOF

    ldapadd -Y EXTERNAL -H 'ldapi:///' -f /etc/openldap/schema/cosine.ldif
    ldapadd -Y EXTERNAL -H 'ldapi:///' -f /etc/openldap/schema/nis.ldif 
    ldapadd -Y EXTERNAL -H 'ldapi:///' -f /etc/openldap/schema/inetorgperson.ldif

    ldapadd -x -w password -D "cn=ldapadm,dc=example,dc=local" <<EOF
dn: dc=example,dc=local
dc: example
o: Example Organization
objectClass: dcObject
objectClass: organization

dn: ou=users,dc=example,dc=local
ou: users
objectClass: top
objectClass: organizationalUnit

dn: ou=groups,dc=example,dc=local
ou: groups
objectClass: top
objectClass: organizationalUnit

dn: cn=hab,ou=groups,dc=example,dc=local
objectClass: posixGroup
objectClass: top
cn: hab
gidNumber: 21000

dn: uid=hab,ou=users,dc=example,dc=local
objectClass: top
objectClass: account
objectClass: posixAccount
objectClass: shadowAccount
cn: hab
uid: hab
uidNumber: 11000
gidNumber: 21000
homeDirectory: /hab
loginShell: /sbin/nologin
EOF

    authconfig --enableldap --enableldapauth --ldapserver=127.0.0.1 --ldapbasedn="dc=example,dc=local" --enablemkhomedir --update
    systemctl restart nslcd

    systemctl start nscd

    umask "$previous_umask"

    if grep 'hab' /etc/passwd;then
        log_error "found hab user in /etc/passwd"
        return 1
    fi

    if grep 'hab' /etc/group;then
        log_error "found hab user in /etc/group"
        return 1
    fi
}

do_test_deploy() {
    do_test_deploy_default
    chef-automate bootstrap bundle create -o bootstrap.abb
}

do_test_restore() {
    do_test_restore_default || return 1
}
