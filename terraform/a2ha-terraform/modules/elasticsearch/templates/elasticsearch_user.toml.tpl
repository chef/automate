[es_yaml]
  [es_yaml.http]
    port = ${listen_port}

  [es_yaml.discovery.zen]
    minimum_master_nodes = ${minimum_masters}

  [es_yaml.discovery.zen.ping.unicast]
    hosts = [ ${private_ips} ]

  [es_yaml.transport]
    host = "${private_ip}"
    tcp.port = 9300
