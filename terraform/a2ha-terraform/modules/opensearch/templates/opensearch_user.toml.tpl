[network]
  port = ${listen_port}
  host = "${private_ip}"

[discovery]
  minimum_master_nodes = ${minimum_masters}
  ping_unicast_hosts = [ ${private_ips} ]

[transport]
  port = 9300
