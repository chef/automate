#!/bin/bash

HA_BACKEND_DIR=$(dirname "${BASH_SOURCE[0]}")
HA_BACKEND_USER="admin"
# changing to match the default in automate-ha-backend
HA_BACKEND_PASSWORD="admin"

ha_backend_container1=$(service_container_name "ha_backend_1")
ha_backend_container2=$(service_container_name "ha_backend_2")

ha_backend_private=$(service_config_path "ha_backend_private")
ha_backend_config=$(service_config_path "ha_backend.toml")
ha_admin_pg_password='thisisapassword%u'

ha_backend_setup() {
    mkdir -p "$ha_backend_private"
    mkdir -p "$(dirname "$ha_backend_config")"
    mkdir -p $(service_config_path "ha_backend_backups")
    chmod -R 0777 $(service_config_path "ha_backend_backups")
    chmod -R 0777 $(service_config_path "")
    local peer_ring_file
    peer_ring_file=$(service_config_path "ha_backend_peers")
    touch "$peer_ring_file"

    local ha_backend_container1_ip ha_backend_container2_ip
    docker_run "$ha_backend_container1" "chefes/centos-systemd:latest"
    ha_backend_container1_ip=$(container_ip "$ha_backend_container1")
    log_info "Launched $ha_backend_container1 with ip ${ha_backend_container1_ip}"
    echo "$ha_backend_container1_ip" >> "$peer_ring_file"

    docker_run "$ha_backend_container2" "chefes/centos-systemd:latest"
    ha_backend_container2_ip=$(container_ip "$ha_backend_container2")
    log_info "Launched $ha_backend_container2 with ip ${ha_backend_container2_ip}"

    echo "Generating elasticsearch ssl keys"

    # copy-pasta'd from a2-ha-backend
    certdir="$HA_BACKEND_DIR/certificates"
    mkdir -p "$certdir"

    #shellcheck disable=SC2034
    RANDFILE="$certdir"/.rnd
    #First, create a private key for the CA:
    openssl genrsa -out "$certdir"/MyRootCA.key 2048

    #Create the CA and enter the Organization details:
    openssl req -x509 -new -key "$certdir"/MyRootCA.key -sha256 -out "$certdir"/MyRootCA.pem -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca'

    #the rsa keys for odfe and postgresql
    openssl genrsa -out "$certdir"/odfe-node-pkcs12.key 2048
    openssl genrsa -out "$certdir"/odfe-admin-pkcs12.key 2048
    openssl genrsa -out "$certdir"/postgresql.key 2048

    #IMPORTANT: Convert these to PKCS#5 v1.5 to work correctly with the JDK.
    openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "$certdir/odfe-node-pkcs12.key" -topk8 -out "$certdir/odfe-node.key" -nocrypt
    openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "$certdir/odfe-admin-pkcs12.key" -topk8 -out "$certdir/odfe-admin.key" -nocrypt

    #Create the CSR and enter the organization and server details for the odfe and postgresql keys
    openssl req -new -key "$certdir/odfe-node.key" -out "$certdir/odfe-${ha_backend_container1}.csr" -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode' -extensions san -reqexts san -config <(echo '[req]'; echo 'distinguished_name=req'; echo '[san]'; echo "subjectAltName=IP:${ha_backend_container1_ip},DNS:${ha_backend_container1_ip}")
    openssl req -new -key "$certdir/odfe-node.key" -out "$certdir/odfe-${ha_backend_container2}.csr" -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode' -extensions san -reqexts san -config <(echo '[req]'; echo 'distinguished_name=req'; echo '[san]'; echo "subjectAltName=IP:${ha_backend_container2_ip},DNS:${ha_backend_container2_ip}")
    openssl req -new -key "$certdir/postgresql.key" -out "$certdir/postgresql.csr" -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefpostgresql' -extensions san -reqexts san -config <(echo '[req]'; echo 'distinguished_name=req'; echo '[san]'; echo "subjectAltName=IP:${ha_backend_container1_ip},IP:${ha_backend_container2_ip},DNS:${ha_backend_container1_ip},DNS:${ha_backend_container2_ip}")

    #Create the CSR and enter the organization and server details for the admin key
    openssl req -new -key "$certdir/odfe-admin.key" -out "$certdir/odfe-admin.csr" -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin'

    #Use the CSR to generate the signed odfe and postgresql Certificates:
    openssl x509 -req -in "$certdir/odfe-${ha_backend_container1}.csr" -CA "$certdir/MyRootCA.pem" -CAkey "$certdir/MyRootCA.key" -CAcreateserial -out "$certdir/odfe-${ha_backend_container1}.pem" -sha256
    openssl x509 -req -in "$certdir/odfe-${ha_backend_container2}.csr" -CA "$certdir/MyRootCA.pem" -CAkey "$certdir/MyRootCA.key" -CAcreateserial -out "$certdir/odfe-${ha_backend_container2}.pem" -sha256
    openssl x509 -req -in "$certdir/postgresql.csr" -CA "$certdir/MyRootCA.pem" -CAkey "$certdir/MyRootCA.key" -CAcreateserial -out "$certdir/postgresql.pem" -sha256

    #Use the CSR to generate the signed admin Certificate:
    openssl x509 -req -in "$certdir/odfe-admin.csr" -CA "$certdir/MyRootCA.pem" -CAkey "$certdir/MyRootCA.key" -CAcreateserial -out "$certdir/odfe-admin.pem" -sha256

    docker cp "$HA_BACKEND_DIR/setup.sh" "${ha_backend_container1}:/setup.sh"
    docker cp "$HA_BACKEND_DIR/setup.sh" "${ha_backend_container2}:/setup.sh"
    docker cp "$certdir" "${ha_backend_container1}:/certificates"
    docker cp "$certdir" "${ha_backend_container2}:/certificates"


    docker exec -t "$ha_backend_container1" /setup.sh "$ha_backend_container1_ip"
    docker exec -t "$ha_backend_container2" /setup.sh "$ha_backend_container2_ip"

    local errcode
    local output
    for try in {1..60}; do
        echo "Trying to create dbuser (attempt #${try})"
        errcode="0"
        output="$(docker exec --env PGPASSWORD="$ha_admin_pg_password" --env HAB_LICENSE=accept-no-persist "$ha_backend_container1" \
	hab pkg exec core/postgresql11 -- psql \
            -h 127.0.0.1 -p 7432 -U admin -d postgres -c \
            "CREATE USER dbuser WITH PASSWORD '$ha_admin_pg_password'")" || errcode="$?"
        if [ "$errcode" -eq "0" ]; then
            break
        else
            echo "Retrying in 5 seconds"
            sleep 5
        fi
    done

    if [ ! "$errcode" -eq "0" ]; then
        echo "Failed to create dbuser($errcode): $output"
        return 1
    fi

    cat <<DOC > "$ha_backend_config"
[global.v1.external.elasticsearch]
enable = true
nodes = ["https://${ha_backend_container1_ip}:9200", "https://${ha_backend_container2_ip}:9200"]

[global.v1.external.elasticsearch.backup]
enable = true
location = "fs"

[global.v1.external.elasticsearch.backup.fs]
path = "/services/ha_backend_backups"

[global.v1.external.elasticsearch.auth]
scheme = "basic_auth"

[global.v1.external.elasticsearch.auth.basic_auth]
username = "${HA_BACKEND_USER}"
password = "${HA_BACKEND_PASSWORD}"

[global.v1.external.elasticsearch.ssl]
# defaults from automate-ha-backend
server_name = "chefnode"
root_cert = """$(cat "${certdir}/MyRootCA.pem")"""

[global.v1.external.postgresql]
enable = true
nodes = ["${ha_backend_container1_ip}:7432", "${ha_backend_container2_ip}:7432"]

[global.v1.external.postgresql.ssl]
enable = true
root_cert = """$(cat "${certdir}/MyRootCA.pem")"""

[global.v1.external.postgresql.auth]
scheme = "password"

[global.v1.external.postgresql.auth.password.superuser]
username = "admin"
password = "$ha_admin_pg_password"
[global.v1.external.postgresql.auth.password.dbuser]
username = "dbuser"
password = "$ha_admin_pg_password"

[global.v1.external.postgresql.backup]
enable = true

[esgateway.v1.sys.service]
host = "0.0.0.0"
DOC

    for try in {1..8}; do
        echo "Testing odfe connectivity (attempt ${try})"

        errcode=0
        curl --connect-timeout 15 --max-time 15 -u admin:admin --cacert "${certdir}/MyRootCA.pem" --key "${certdir}/odfe-admin.key" --cert "${certdir}/odfe-admin.pem" --resolve "chefnode:9200:${ha_backend_container1_ip}" "https://chefnode:9200" &&
        curl --connect-timeout 15 --max-time 15 -u admin:admin --cacert "${certdir}/MyRootCA.pem" --key "${certdir}/odfe-admin.key" --cert "${certdir}/odfe-admin.pem" --resolve "chefnode:9200:${ha_backend_container2_ip}" "https://chefnode:9200" || errcode=${?}
        if [ ${errcode} -eq 0 ]; then
            break
        else
            if [ ${try} -eq 8 ]; then
                echo "Failed to validate odfe connectivity"
                return 1
            fi
            sleep 15
        fi
    done
}

ha_backend_dump_logs() {
    tmpdir=$(mktemp -d)
    docker exec -t "$ha_backend_container1" journalctl --no-pager -u hab-sup > "$tmpdir/ha_backend_container_1"
    docker exec -t "$ha_backend_container2" journalctl --no-pager -u hab-sup > "$tmpdir/ha_backend_container_2"

    if command -v buildkite-agent; then
        if ! buildkite-agent artifact upload "$tmpdir/*"
        then
            echo "Failed to ha backend container logs"
        fi
    fi
    rm -r "$tmpdir"
}

ha_backend_teardown() {
    docker stop "$ha_backend_container1"
    docker stop "$ha_backend_container2"
}
