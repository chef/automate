variable "automate_admin_email" {
  description = "Email address for the automate admin user account"
  default     = "admin@example.com"
}

variable "automate_admin_username" {
  description = "User name for the automate admin user account"
  default     = "admin"
}

variable "automate_admin_password" {
  description = "Password for the automate admin user account"
  default     = "c0d3c@n!"
}

variable "automate_config_file" {
  description = "Custom config file to use when setting up automate"
  default     = ""
}

variable "automate_dc_token" {
  description = "Token for the Automate Data Collector API"
  default     = "93a49a4f2482c64126f7b6015e6b0f30284287ee4054ff8807fb63d9cbd1c506"
}

variable "automate_archive_disk_fs_path" {
  default     = "/mnt/automate_backups/automate"
  description = "The filesystem path to the Automate frontend archive share."
}

variable "artifact_info" {
  default     = ""
  description = "This variable is auto populated."
}

variable "automate_fqdn" {
  default     = ""
  description = "Automate FQDN variable. Leave as an empty string for AWS"
}

variable "automate_instance_count" {
  default = 1
}

variable "automate_license" {
  default = "Contact Chef Sales at sales@chef.io to request a license."
}

variable "backend_aib_dest_file" {
  default     = ""
  description = "The full path of the airgap bundle destination file."
}

variable "backend_aib_local_file" {
  default     = ""
  description = "The filename of the airgap bundle to use from the 'terraform/transfer_files' directory."
}

variable "chef_server_instance_count" {
  default = 1
}

variable "curator_pkg_ident" {
  default = "chef/automate-backend-curator"
}

variable "curator_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "elasticsearch_archive_disk_fs_path" {
  default     = "/mnt/automate_backups/elasticsearch"
  description = "The filesystem path to the Elasticsearch archive share."
}

variable "elasticsearch_https_user" {
  default = "automate_elasticsearch"
}

variable "elasticsearch_instance_count" {
  default = 3
}

variable "elasticsearch_listen_port" {
  default = 9200
}

variable "elasticsearch_pkg_ident" {
  default = "chef/automate-backend-elasticsearch"
}

variable "elasticsearch_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "elasticsidecar_pkg_ident" {
  default = "chef/automate-backend-elasticsidecar"
}

variable "elasticsidecar_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "frontend_aib_dest_file" {
  default     = ""
  description = "The full path of the airgap bundle destination file."
}

variable "frontend_aib_local_file" {
  default     = ""
  description = "The filename of the airgap bundle to use from the 'terraform/transfer_files' directory."
}

variable "habitat_info" {
  default     = ""
  description = "This variable is auto populated."
}

variable "habitat_uid_gid" {
  default     = ""
  description = "Set UID/GID for hab user"
}

variable "hab_sup_http_gateway_auth_token" {
  description = "Simple HTTP authentication Bearer token. Run 'make sup-keys' from the top-level project directory."
}

variable "hab_sup_http_gateway_priv_key" {
  default = <<CERT
-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEAwLxmHjJKMBmpQvLfQ00GPil9Dxn1qzHPpg/s1QMB/vkyluZG
MkalMwuPJKb3UgVcVopfVqbig4/I5KLDmj6C8GoO9Pj969ndUyyisj3d6RdMp6HJ
zOd6ccKBifPLnhgSPQAK76B+G76I/yKbLTr1I0VZaoOlCZOE2CnQz2jUbYIfb2eY
a5wCwh6IdeHSYR6KNEgDB25nobI0C8igSWuUqXZlFWnBKEfICvLSpbBvs7xH6GB5
vBR8tQeWWkgSoZ4tptgTtBKFkYP6gpJjZOg1oKcctwz+sHwwpg57XnOnKpbGaCbn
hIhLDGSAPmiSwZJjbVDXEgUL9mM3Hlk6ySWdvQIDAQABAoIBAHfYruc3/knKqyWm
QI5o5ujgg5NQYSMC5kwsuuf55tPVwU90AVJh+3eySGBSMpgeHTzy7jXDsDZ+wjzp
uRfAp0s3p/VMnxh5cNa3erO7JLI8er4CLKnthoYWnNZbctHrxeuIi0RWWBSIdkew
DS4i6yvzg5ZoJce+o2yHFADk/rLYke0Mjz1jDeC+ol8GRNL+e0Z/D151WsTKNAQs
NdAitz0jkszT4cybTFld6MJo80vjsIWneFOn/Ig4AL/ODKNzz45WF4F3vup5meK/
7LxnwLdbvPMGIdgiUhCPlKoPjJ4iR8z2rzu7WDubNyQJEjyOod7533K3XaIdTElQ
eueGkAECgYEA8M/Yfn4xIp+Atiu8rPoSIcVZ3RMKX2B/7XG4+Og8CkymodbESwbt
s2jw8tqd9Mb3QosBBViWiEYYvbyXu7Xx7Rd+74qmJAuvjkrR0C0fp7xcqpEvU/kI
7g0KMJ6iG2R15/G9mUNV+hZt/QabnV/mewRxJwfg+RcyLEvtbSSuH0ECgYEAzORR
T4mYozO5pKwwYQwqkmpXpbgOCbLXqRy5JvNGNaIYiFfVHKxGhntYzWXY8875gorA
3KYHJnx5MVFO96CWuVwnJqCIa8LJPRnwhZJQqdbYPS/MDAWFSbdCk1uVTF3ONlA6
FvVyHY5qYkFZB9cP7PKZipIWtCGpKYlez5o0m30Cf1aKW0MaFo5aw+az0RpYxCte
wOyx6whbpZ2vQMoyq2EIKtn+VIyh8MqI7A5agml1F2bSjYteSrqudjW5kM3klQyf
ZP6w1QQfLvfz63LkxHuDNcoWYuvKk2r5RLA0Ug2GDNlOBtUNXdiCNwS3WzUzQaDz
97YPHspli6JfpvOSOwECgYEAjSYMzdpgiK2tikAE5BZZjZws3UE6invmDd53vtmA
2JnehK7dZW9Y9nQq7PTozK0dPk0QJ/FIo77ETrFQ0U6UBwXxAq88y9/YentQMvRl
FVuohB5VV0LGW84f0xwYcg99a/VAz2gvQSVHLIEQMFJ76iQGmmuhsF4wBylXY3U8
IJECgYEA0xwjZGi+nZ0Wsf+RHwZTTgzLy4vkva+1YRKQzApO1O311heCETMzGUPY
kjB+wCoTUim2j9VPDwFS0mnsoTofpyU+PbXPv9X1aO76GrN+VytlVcNkE8QuTH1w
MrlWwifcKKXSqePH8lRpnV0aJifj+R5TPlip7c6aL0qNgoJW9kQ=
-----END RSA PRIVATE KEY-----
CERT


  description = "The RSA private key of TLS public cert used for the HTTP gateway in PEM format."
}

variable "hab_sup_http_gateway_pub_cert" {
  default = <<CERT
-----BEGIN CERTIFICATE-----
MIIDQjCCAioCCQC+Wis5dMNsUDANBgkqhkiG9w0BAQsFADBlMQswCQYDVQQGEwJV
UzETMBEGA1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UE
CgwRQ2hlZiBTb2Z0d2FyZSBJbmMxEzARBgNVBAMMCmNoZWZyb290Y2EwHhcNMTkw
NjE0MTQ0NjM4WhcNMjkwNDIyMTQ0NjM4WjBhMQswCQYDVQQGEwJVUzETMBEGA1UE
CAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2VhdHRsZTEaMBgGA1UECgwRQ2hlZiBT
b2Z0d2FyZSBJbmMxDzANBgNVBAMMBmhhYnN1cDCCASIwDQYJKoZIhvcNAQEBBQAD
ggEPADCCAQoCggEBAMC8Zh4ySjAZqULy30NNBj4pfQ8Z9asxz6YP7NUDAf75Mpbm
RjJGpTMLjySm91IFXFaKX1am4oOPyOSiw5o+gvBqDvT4/evZ3VMsorI93ekXTKeh
ycznenHCgYnzy54YEj0ACu+gfhu+iP8imy069SNFWWqDpQmThNgp0M9o1G2CH29n
mGucAsIeiHXh0mEeijRIAwduZ6GyNAvIoElrlKl2ZRVpwShHyAry0qWwb7O8R+hg
ebwUfLUHllpIEqGeLabYE7QShZGD+oKSY2ToNaCnHLcM/rB8MKYOe15zpyqWxmgm
54SISwxkgD5oksGSY21Q1xIFC/ZjNx5ZOsklnb0CAwEAATANBgkqhkiG9w0BAQsF
AAOCAQEApIxWhHmMhzW6ut2g9js5e3qzvT2CP5EuHaA1jyS6adkn6bbbvWFsxdS/
rL0b+3NuafcgC8TKaMi5VbabfMJKqNWRdaLDkhGVZDCSAyZYn02/YAd2xpnmLoOp
M3CGRJ5Wjy0DFNOwYlReUPAZZnmTV9R68Sr+ynh9mlabRkpsFwhoqz5VJFuexihB
V0nOFjsqloDFTnkzble/iAJ9BpoBZa9mnkGggKPHgClDznBuF44IpzDnErOSB89H
1UuLJRPVIx4KlUpvyGBw6yvMQKuKGqh9GCtMavsg0W71gO6dpGsfFKbIN/AXZwpy
/8LvWjJDeacokzLoSdcR0Kdqvok6tg==
-----END CERTIFICATE-----
CERT


  description = "The TLS public cert used for the HTTP gateway in PEM format."
}

variable "hab_sup_http_gateway_ca_cert" {
  default = <<CERT
-----BEGIN CERTIFICATE-----
MIIDnTCCAoWgAwIBAgIJAONizKC0sDtyMA0GCSqGSIb3DQEBCwUAMGUxCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRow
GAYDVQQKDBFDaGVmIFNvZnR3YXJlIEluYzETMBEGA1UEAwwKY2hlZnJvb3RjYTAe
Fw0xOTA2MTQxNDQzMTBaFw0yOTA0MjIxNDQzMTBaMGUxCzAJBgNVBAYTAlVTMRMw
EQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMRowGAYDVQQKDBFD
aGVmIFNvZnR3YXJlIEluYzETMBEGA1UEAwwKY2hlZnJvb3RjYTCCASIwDQYJKoZI
hvcNAQEBBQADggEPADCCAQoCggEBALZkMfpexXfpuRyeHbDslW+kYFqBqIz9AIQZ
r3DiN+/+jsu1mgm7K4PXlDMCYFARL/P3XkEw8do4Twldp5wxc6ZqygnkU+4ZT39v
tPLG1xYQdTt1sclwgtn0vekTswhBCpkuvK/sKJftGuI3IYhUisZlJep6fgWs+6j7
4FqnD5+zkWptGMf4M94bwMe43utxDimLrWakG06IZ71OwwBx4SXL6MEDcVJMvlAE
SxIyhgCP+zhImb9y8ADuzt+GfwScsPA1uBTzdgNdR9+ad4gn5FbAwy/B3QjMuFeS
Uvt/xrgzEs2AyQeOiCXsPtGmUvnLKVYwYhCWh3odj/9NBqPfP8cCAwEAAaNQME4w
HQYDVR0OBBYEFFIXTcBues6jY0AoMiHttCUUVS9oMB8GA1UdIwQYMBaAFFIXTcBu
es6jY0AoMiHttCUUVS9oMAwGA1UdEwQFMAMBAf8wDQYJKoZIhvcNAQELBQADggEB
AA9oyqBJt0zFadUxpJqnH5Dd3Izl/fS4n7SBGozzUiYmcyR/xeJUeiyOifqtkbf6
gWnOpLojz0yXsA+T5bxvTYi01yNrTd+sRbRovoWW0FTH/ZCHzNpZDT9HLLU3dM/B
Bnsj9x+sWdzBkJvb4xSgpU9msR49a+oFOpJiv4Ru8JT1AAgmbFSUBlhrhDGpacqs
Yzsj9J7kbzAZQPymEr8MYRnZLcxsrRgZZkP4F+rMZIUs8AVRDVTSQHRKiPsIpDBY
oP9SRV/JN906bh2HrS9E5VLsAzisAwi4v3bJYEYVTEeH4A37qQMcKpjilL9jyMNJ
ihh549H5m2kKcTMu/Qf2Kk0=
-----END CERTIFICATE-----
CERT


  description = "Issuer of the TLS cert used for the HTTP gateway in PEM format."
}

variable "hab_sup_ring_key" {
  description = "A symmetric pre-shared key used for wire encryption. Run 'make sup-keys' from the top-level project directory."
}

variable "hab_sup_run_args" {
  default = "--no-color --strategy none --peer-watch-file /etc/hab_peer_watch --permanent-peer --key /hab/sup/default/HttpGateway.key --certs /hab/sup/default/HttpGatewayChained.pem"
}

variable "journalbeat_pkg_ident" {
  default = "chef/automate-backend-journalbeat"
}

variable "kibana_https_user" {
  default = "admin"
}

variable "kibana_pkg_ident" {
  default = "chef/automate-backend-kibana"
}

variable "metricbeat_pkg_ident" {
  default = "chef/automate-backend-metricbeat"
}

variable "nfs_mount_path" {
  default     = "/mnt/automate_backups"
  description = "The NFS mount base path for backups and archives."
}

variable "pgleaderchk_listen_port" {
  default = 6432
}

variable "pgleaderchk_pkg_ident" {
  default = "chef/automate-backend-pgleaderchk"
}

variable "pgleaderchk_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "postgresql_archive_disk_fs_path" {
  default     = "/mnt/automate_backups/postgresql"
  description = "The filesystem path to the PostgreSQL archive share."
}

variable "postgresql_instance_count" {
  default = 3
}

variable "postgresql_listen_port" {
  default = 5432
}

variable "postgresql_pkg_ident" {
  default = "chef/automate-backend-postgresql"
}

variable "postgresql_pg_dump_enabled" {
  default     = true
  description = "Enable or disable PostgreSQL pg_dump for backups. possible values: [true, false]"
}

variable "postgresql_ssl_enable" {
  default     = true
  description = "Enable or disable SSL in PostgreSQL"
}

variable "postgresql_svc_load_args" {
  default = "--topology leader --strategy none"
}

variable "postgresql_wal_archive_enabled" {
  default     = false
  description = "(This is ALPHA and known to not work yet.) Enable or disable PostgreSQL WAL archive mode. possible values: [true, false]"
}

variable "proxy_listen_port" {
  default = 7432
}

variable "proxy_pkg_ident" {
  default = "chef/automate-backend-haproxy"
}

variable "proxy_svc_load_args" {
  default = "--topology standalone --strategy none"
}

variable "rsync_files" {
  default     = []
  description = "An ordered array of SRC1,DST1,SRC2,DST2 file pairs. SRC is relative to the terraform/transfer_files directory"
}

variable "teams_port" {
  default = 10128
}

variable "tmp_path" {
  default = "/var/tmp"
}

variable "sudo_password" {
  default = ""
}

variable "fe_sudo_password" {
  default = null
}

variable "be_sudo_password" {
  default = null
}

locals {
  fe_sudo_password = var.fe_sudo_password != null ? var.fe_sudo_password : var.sudo_password
  be_sudo_password = var.be_sudo_password != null ? var.be_sudo_password : var.sudo_password
}
