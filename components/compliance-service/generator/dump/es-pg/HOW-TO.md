
# ES5 dump created in Automate 1 via:
```
cd /var/opt/delivery/elasticsearch/data/
tar -czvf /tmp/es5-a1-data.tar.gz *
```

# ES6 dump created in Automate 2 via:
```
cd ~/git/a2/components/compliance-service/elasticsearch/.tmp/esdata6/
tar -czvf ~/git/a2/components/compliance-service/generator/dump/es-pg/es6-a2v2-data.tar.gz *
```

# Postgres 9.3 dump created in Automate 1 using:
```
sudo -u chef-pgsql /opt/delivery/embedded/bin/pg_dump delivery --no-owner \
  -t s_tags -t s_secrets -t s_secrets_tags \
  -t agents -t node_managers -t results -t profiles -t tags \
  -t jobs -t jobs_nodes -t jobs_profiles -t jobs_tags \
  -t nodes -t nodes_agents -t nodes_secrets -t nodes_tags | gzip --fast > /tmp/a1-pg-dump.sql.gz
```

For all the secrets encrypted inside this backup the `secret_key` is `f6326e5a27876d9d39815c64979611a7`
