+++
title = "External PostgreSQL Upgrade Steps"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "External PostgreSQL Upgrade Steps"
    parent = "automate/reference"
    identifier = "automate/reference/external_postgres_upgrade_steps.md.md External PostgreSQL Upgrade Steps"
    weight = 60
+++

## Upgrade PostreSQL from v9.6 to 13
Please follow these steps only if you have a setup with below given conditions:
- If you are running PostgreSQL v9.6
- On Ubuntu 18.04 or latest
- On a virtual machine like EC2 or independent Physical Machine
- Ensure you have enough free space to run the duplicate copy of the data present in current PostgreSQL 9.6, i.e. you have more than 55% free space on the machine.
### Steps to Upgrade PostgreSQL:
1. SSH or Login to the machine running PostgreSQL 9.6 with user who has sudo or root access.
    ```shell
    ssh -i "<xyz.pem>" <sudo_username>@<postgres_machine_ip>
    ```
2. Install PostgreSQL v13
    ```shell
    sudo apt-get update
    sudo apt-get install postgresql-13
    ```
3. If the PostgreSQL v9.6 config was modified or customized. Please check the differences and update the new PostgreSQL v13 config with similar changes. Please connect with your database administrator if you don't know these changes.
    ```shell
    sudo sdiff -s /etc/postgresql/9.6/main/postgresql.conf /etc/postgresql/13/main/postgresql.conf
    sudo sdiff -s /etc/postgresql/9.6/main/pg_hba.conf /etc/postgresql/13/main/pg_hba.conf
    ```
4. Stop Both PostgreSQL Servers:
    ```shell
    sudo systemctl stop postgresql.service
    ```
5. Login as `postgres` User
    ```shell
    sudo su - postgres
    ```
6. Run Vacuum DB before moving data from PostgreSQL v9.6 to v13
    ```shell
    vacuumdb --all --full
    ```
    Expected output:
    ```shell
    vacuumdb: vacuuming database "automate-cs-bookshelf"
    vacuumdb: vacuuming database "automate-cs-oc-bifrost"
    vacuumdb: vacuuming database "automate-cs-oc-erchef"
    vacuumdb: vacuuming database "chef_applications_service"
    vacuumdb: vacuuming database "chef_authn_service"
    vacuumdb: vacuuming database "chef_authz_service"
    vacuumdb: vacuuming database "chef_cereal_service"
    vacuumdb: vacuuming database "chef_compliance_service"
    vacuumdb: vacuuming database "chef_config_mgmt_service"
    vacuumdb: vacuuming database "chef_infra_proxy"
    vacuumdb: vacuuming database "chef_ingest_service"
    vacuumdb: vacuuming database "chef_license_control_service"
    vacuumdb: vacuuming database "chef_session_service"
    vacuumdb: vacuuming database "chef_teams_service"
    vacuumdb: vacuuming database "chef_user_settings_service"
    vacuumdb: vacuuming database "data_feed_service"
    vacuumdb: vacuuming database "dex"
    vacuumdb: vacuuming database "nodemanager_service"
    vacuumdb: vacuuming database "notifications_service"
    vacuumdb: vacuuming database "postgres"
    vacuumdb: vacuuming database "secrets_service"
    vacuumdb: vacuuming database "template1"
    ```
7. Run pg_upgrade check command.
    ```shell
    cd /tmp
    /usr/lib/postgresql/13/bin/pg_upgrade \
    --old-datadir=/var/lib/postgresql/9.6/main \
    --new-datadir=/var/lib/postgresql/13/main \
    --old-bindir=/usr/lib/postgresql/9.6/bin \
    --new-bindir=/usr/lib/postgresql/13/bin \
    --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
    --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf' \
    --check
    ```
8. Migrate the Data (run pg_upgrade command without --check):
    ```shell
    cd /tmp
    /usr/lib/postgresql/13/bin/pg_upgrade \
    --old-datadir=/var/lib/postgresql/9.6/main \
    --new-datadir=/var/lib/postgresql/13/main \
    --old-bindir=/usr/lib/postgresql/9.6/bin \
    --new-bindir=/usr/lib/postgresql/13/bin \
    --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
    --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf'
    ```
9. Exit postgres user
    ```shell
    exit
    ```
10. Swap the ports of PostgreSQL v9.6 and v13:
    ```shell
    PG_9_6_PORT=5432 # Assuming Postgres v9.6 is currently running on this port
    PG_13_PORT=5433 # Assuming Postgres 13 is currently running on this port
    sudo sed -i "s/port = $PG_9_6_PORT/port = $PG_13_PORT/g" /etc/postgresql/9.6/main/postgresql.conf
    sudo sed -i "s/port = $PG_13_PORT/port = $PG_9_6_PORT/g" /etc/postgresql/13/main/postgresql.conf
    ```
11. Start back both PostgreSQL Servers: 
    ```shell
    sudo systemctl start postgresql.service
    ```
12. Check version:
    ```shell
    sudo su - postgres
    psql -c "SELECT version();"
    ```
13. Run the generated `analyze_new_cluster.sh` script
    ```shell
    ./analyze_new_cluster.sh
    ```
    Reindex is not required for Chef Automate usecase. But if pg_upgrade reported errors or need for reindexing please refer to [pg_upgrade documentation](https://www.postgresql.org/docs/13/pgupgrade.html) for details.
14. Exit postgres user:
    ```shell
    exit
    ```

### Remove PostgreSQL v9.6 from machine
After you have confirmed, you no longer need PostgreSQL v9.6 and clean up the space use by it.\
Remove PostgreSQL v9.6 and data:
```shell
sudo su - postgres
./delete_old_cluster.sh
```
