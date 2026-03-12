# OpenLDAP + LDAPS + Chef Automate/dex Setup Guide

## 1. Prepare the Server
- SSH into your Ubuntu EC2 instance.
- Update packages:
  ```sh
  sudo apt-get update
  ```

## 2. Open Firewall Ports
- In AWS EC2 Security Groups, allow inbound traffic for:
  - Port 636 (LDAPS)
  - Port 389 (LDAP, if needed for StartTLS or initial setup)

## 3. Install OpenLDAP and Utilities
```sh
sudo apt-get install slapd ldap-utils
```
- Set admin password (e.g., `123`).

## 4. Configure OpenLDAP
```sh
sudo dpkg-reconfigure slapd
```
- Omit OpenLDAP server configuration? **No**
- DNS domain name: Use your EC2 public DNS (e.g., `ec2-18-191-215-176.us-east-2.compute.amazonaws.com`)
- Organization name: Any (e.g., `EXAMPLE`)
- Admin password: (same as above)
- Database backend: HDB
- Remove database when slapd is purged? **No**
- Move old database? **Yes**
- Allow LDAPv2 protocol? **No**

## 5. Generate Self-Signed Certificate with SAN
Create an OpenSSL config file (e.g., `ldap-cert.conf`) with SAN for your hostname:
```
[req]
distinguished_name = req_distinguished_name
req_extensions = v3_req
prompt = no

[req_distinguished_name]
CN = ec2-18-191-215-176.us-east-2.compute.amazonaws.com

[v3_req]
subjectAltName = @alt_names

[alt_names]
DNS.1 = ec2-18-191-215-176.us-east-2.compute.amazonaws.com
```
Generate key and cert:
```sh
openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
  -keyout /etc/ldap/ssl/ldap-server.key \
  -out /etc/ldap/ssl/ldap-server.crt \
  -config ldap-cert.conf
```

## 6. Set Permissions and AppArmor
```sh
sudo chown openldap:openldap /etc/ldap/ssl/ldap-server.*
sudo chmod 600 /etc/ldap/ssl/ldap-server.*
```
- If using AppArmor, add `/etc/ldap/ssl/* r,` and `/etc/ldap/ssl/* k,` to `/etc/apparmor.d/local/usr.sbin.slapd` and reload AppArmor.

## 7. Update slapd TLS Config
```sh
sudo ldapmodify -Y EXTERNAL -H ldapi:/// <<EOF
dn: cn=config
changetype: modify
replace: olcTLSCertificateFile
olcTLSCertificateFile: /etc/ldap/ssl/ldap-server.crt
-
replace: olcTLSCertificateKeyFile
olcTLSCertificateKeyFile: /etc/ldap/ssl/ldap-server.key
EOF
```

## 8. Restart slapd
```sh
sudo systemctl restart slapd
```

## 9. Test LDAPS
```sh
openssl s_client -connect ec2-18-191-215-176.us-east-2.compute.amazonaws.com:636
```
- Should show a valid handshake and your certificate with correct CN/SAN.

## 10. Prepare LDIF File for Users
Create `add_entries.ldif` with contents like:
```
dn: ou=People,dc=ec2-18-191-215-176,dc=us-east-2,dc=compute,dc=amazonaws,dc=com
objectClass: organizationalUnit
ou: People

dn: cn=alice,ou=People,dc=ec2-18-191-215-176,dc=us-east-2,dc=compute,dc=amazonaws,dc=com
objectClass: person
objectClass: inetOrgPerson
objectClass: simpleSecurityObject
userPassword: 123
sn: alice
cn: alice

... (repeat for other users)
```

## 11. Import Users
```sh
ldapadd -x -D "cn=admin,dc=ec2-18-191-215-176,dc=us-east-2,dc=compute,dc=amazonaws,dc=com" -W -f add_entries.ldif
```
- Enter admin password when prompted.

## 12. Update Chef Automate/dex Config
In your `config.toml` or `auth.toml`:
```toml
[dex.v1.sys.connectors.ldap]
  ca_contents = """<your CA certificate>"""
  host = "ec2-18-191-215-176.us-east-2.compute.amazonaws.com:636"
  bind_dn = "cn=admin,dc=ec2-18-191-215-176,dc=us-east-2,dc=compute,dc=amazonaws,dc=com"
  bind_password = "123"
  insecure_no_ssl = false
  user_id_attr = "cn"
  username_attr = "cn"
  base_user_search_dn = "ou=People,dc=ec2-18-191-215-176,dc=us-east-2,dc=compute,dc=amazonaws,dc=com"
  base_group_search_dn = "ou=Groups,dc=ec2-18-191-215-176,dc=us-east-2,dc=compute,dc=amazonaws,dc=com"
  user_display_name_attr = "cn"
```

## 13. Apply Chef Automate Config
```sh
chef-automate config set config.toml
```
or
```sh
chef-automate config patch auth.toml
```

## 14. Test Login
- Go to your Chef Automate UI.
- Log in with one of the LDAP users (e.g., alice / 123).

---
