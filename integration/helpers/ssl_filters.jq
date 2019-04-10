map(select(
         # General Exceptions
         (.id != "LUCKY13") and                           # CBC Ciphers still in use in Golang and Nginx
         (.id != "cert_revocation") and                   # We don't do cert revocation
         (.id != "scanTime") and                          # Unsure why we get this

         # These are either hard to check with client auth enabled or
         # are HTTP only vulnerabilities so we check them on 443 only
         (.id != "secure_client_renego" or .port == "443") and
         (.id != "BREACH" or .port == "443") and

         # TODO notifications-service
         (.port != "10125") and

         # automate-load-balancer
         (.id != "cert_trust" or .port != "443") and            # Test uses self-signed cert
         (.id != "cert_caIssuers" or .port != "443") and        # Test uses self-signed cert
         (.id != "cert_chain_of_trust" or .port != "443") and   # Test uses self-signed cert
         (.id != "cipherlist_128Bit" or .port != "443") and     # For browser compatibility (untested)
         (.id != "HSTS" or .port != "443") and                  # TODO

         # automate-gateway
         (.id != "HSTS" or .port != "2000") and                 # HSTS doesn't seem relevant for this API

         # automate-ui
         (.id != "HSTS" or .port != "10161") and                # TODO (maybe solve in automate-lb instead)
         (.id != "security_headers" or .port != "10161") and    # TODO (maybe solve in automate-lb instead)

         # session-service
         (.id != "HSTS" or .port != "10115") and                # TODO (maybe solve in automate-lb instead)

         # automate-cs-nginx
         (.id != "HSTS" or .port != "10200") and                # TODO (maybe solve in automate-lb instead)
         (.id != "security_headers" or .port != "10200") and    # TODO (maybe solve in automate-lb instead)

         # automate-dex
         (.id != "HSTS" or .port != "10117") and                # TODO (maybe solve in automate-lb instead)
         # TODO
         (.id != "cipherlist_128Bit" or (.port != "10116" and .port != "10117")) and
         (.id != "cipherlist_3DES" or (.port != "10116" and .port != "10117")) and
         (.id != "SWEET32" or (.port != "10116" and .port != "10117")) and

         # backup-gateway
         #
         # minio specifies a rather restricted cipher list that is
         # even stronger than the "HIGH" list in our SSL checker:
         #
         # https://github.com/minio/minio/blob/3e124315c8abd0e1fd33ffd9a7ca3c6314ad1032/cmd/http/server.go#L163-L213
         #
         # since the SSL checker has a fixme next to this, we ignore
         # it:
         #
         # https://github.com/drwetter/testssl.sh/blob/7f8a0f2c8bdea8feba2ff57494fbb49784a80682/testssl.sh#L3069-L3071
         #
         #
         (.id != "cipherlist_HIGH" or .port != "10143") and
         (.id != "HSTS" or .port != "10143") and

         # automate-cs erlang-services
         # TODO: erlang services use common prime
         (.id != "LOGJAM-common_primes" or (.port != "10201" and .port != "10202" and .port != "10203")) and
         (.id != "cipherlist_128Bit" or (.port != "10201" and .port != "10202" and .port != "10203"))
))
