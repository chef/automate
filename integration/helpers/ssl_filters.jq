map(select(
         # General Exceptions
         (.id != "cert_revocation") and                   # We don't do cert revocation
         (.id != "scanTime") and                          # Unsure why we get this
         (.id != "sessionresumption_ID") and              # Go doesn't implement session resumption IDs
         (.severity != "DEBUG") and

         # These are either hard to check with client auth enabled or
         # are HTTP only vulnerabilities so we check them on 443 only
         (.id != "secure_client_renego" or .port == "443") and
         (.id != "BREACH" or .port == "443") and

         # The checker wants your certs to be valid for less than 398 days. We
         # don't think this rule is valuable for the internal A2 CA
         (.id != "cert_validityPeriod" or .port == "443" ) and
         (.id != "cert_extlifeSpan" or .port == "443") and

         # TODO notifications-service
         (.port != "10125") and

         # automate-load-balancer
         (.id != "cert_trust" or .port != "443") and            # Test uses self-signed cert
         (.id != "cert_caIssuers" or .port != "443") and        # Test uses self-signed cert
         (.id != "cert_chain_of_trust" or .port != "443") and   # Test uses self-signed cert
         (.id != "cert_validityPeriod" or .port != "443") and   # Test uses self-signed cert
         (.id != "cert_extlifeSpan" or .port != "443") and      # Test uses self-signed cert

         # automate-builder-memcache doesn't allow us to set server cipher order
         (.id != "cipher_order" or .port != "10102") and

         # security headers
         #
         # automate-lb is responsible for adding required headers for
         # now
         (.id != "HSTS" or .port != "2000") and
         (.id != "HSTS" or .port != "10104") and
         (.id != "HSTS" or .port != "10106") and
         (.id != "HSTS" or .port != "10115") and
         (.id != "HSTS" or .port != "10117") and
         (.id != "HSTS" or .port != "10161") and
         (.id != "HSTS" or .port != "10143") and
         (.id != "HSTS" or .port != "10200") and
         (.id != "security_headers" or .port != "10161") and
         (.id != "security_headers" or .port != "10200") and
         (.id != "security_headers" or .port != "10104") and

         # automate-cs erlang-services
         # TODO: erlang services use common prime
         (.id != "LOGJAM-common_primes" or (.port != "10201" and .port != "10202" and .port != "10203")) and
         (.id != "cipherlist_AVERAGE" or (.port != "10201" and .port != "10202" and .port != "10203")) and
         (.id != "LUCKY13" or (.port != "10201" and .port != "10202" and .port != "10203"))
))
