+++
title = "Security Best Practices"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Security Best Practices"
    parent = "automate/reference"
    identifier = "automate/reference/security_best_practices.md Security Best Practices"
    weight = 50
+++

## TLS/SSL Security Best Practices

### Use Modern TLS Protocols

Chef Automate supports TLS 1.2 and TLS 1.3. For optimal security, use the following configuration:

#### Recommended Configuration (Production)

```toml
[load_balancer.v1.sys.ngx.http]
ssl_protocols = "TLSv1.2 TLSv1.3"
ssl_ciphers = "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256"
ssl_ciphersuites = "TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256"
```

This configuration:
-  Supports both TLS 1.2 (for legacy clients) and TLS 1.3 (for modern clients)
-  Prioritizes strongest ciphers
-  Provides forward secrecy with ECDHE key exchange
-  Disables weak and deprecated protocols

#### High Security Configuration (TLS 1.3 Only)

For environments where all clients support TLS 1.3:

```toml
[load_balancer.v1.sys.ngx.http]
ssl_protocols = "TLSv1.3"
ssl_ciphersuites = "TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256"
```

### Deprecated and Insecure Protocols

{{< warning >}}
**NEVER use the following protocols in production:**

- **SSLv2** - Critically insecure, not supported by modern OpenSSL
- **SSLv3** - Multiple vulnerabilities (POODLE), not supported by modern OpenSSL
- **TLSv1** - Deprecated, not supported by modern OpenSSL (1.1.0+)
- **TLSv1.1** - Deprecated by major browsers and standards, not supported by modern OpenSSL

These protocols are non-functional even if configured, as they are not supported by OpenSSL 1.1.0 and newer.
{{< /warning >}}

### TLS Cipher Selection Guidelines

#### TLS 1.3 Cipher Suites (Priority Order)

1. **`TLS_AES_256_GCM_SHA384`** (Highest Security)
   - AES-256 encryption with GCM mode
   - 384-bit SHA hash
   - Best for environments requiring maximum security

2. **`TLS_CHACHA20_POLY1305_SHA256`** (Mobile Optimized)
   - ChaCha20 stream cipher with Poly1305 MAC
   - Optimized for ARM processors (mobile devices)
   - Excellent performance without AES hardware acceleration

3. **`TLS_AES_128_GCM_SHA256`** (High Performance)
   - AES-128 encryption with GCM mode
   - Lower computational overhead
   - Still provides strong security

#### TLS 1.2 Ciphers (Priority Order)

1. **ECDHE-ECDSA-AES256-GCM-SHA384** / **ECDHE-RSA-AES256-GCM-SHA384**
   - Elliptic Curve Diffie-Hellman Ephemeral (forward secrecy)
   - AES-256-GCM (strongest symmetric encryption)
   - Preferred for high-security environments

2. **ECDHE-ECDSA-CHACHA20-POLY1305** / **ECDHE-RSA-CHACHA20-POLY1305**
   - Forward secrecy with ECDHE
   - ChaCha20-Poly1305 (optimized for non-AES-hardware)
   - Excellent for mobile and IoT devices

3. **ECDHE-ECDSA-AES128-GCM-SHA256** / **ECDHE-RSA-AES128-GCM-SHA256**
   - Forward secrecy with ECDHE
   - AES-128-GCM (high performance, strong security)
   - Good balance of security and performance

### Testing TLS Configuration

#### Verify TLS 1.3 Support

```bash
# Test TLS 1.3 connection
openssl s_client -connect your-automate-fqdn:443 -tls1_3

# Expected output should show:
# - Protocol: TLSv1.3
# - Cipher: TLS_AES_256_GCM_SHA384 (or configured cipher)
```

#### Verify TLS 1.2 Support

```bash
# Test TLS 1.2 connection
openssl s_client -connect your-automate-fqdn:443 -tls1_2

# Expected output should show:
# - Protocol: TLSv1.2
# - Cipher: ECDHE-RSA-AES256-GCM-SHA384 (or configured cipher)
```

#### Verify Weak Protocols are Disabled

```bash
# These should fail with connection errors:
openssl s_client -connect your-automate-fqdn:443 -ssl3     # Should fail
openssl s_client -connect your-automate-fqdn:443 -tls1     # Should fail
openssl s_client -connect your-automate-fqdn:443 -tls1_1   # Should fail
```

## Ensuring autocomplete remains disabled on the login screen.

A configuration maintained in Chef Automate's nginx.conf file determines the autocomplete functionality.

    add_header Cache-Control "no-cache, no-store, must-revalidate";
    

Please ensure you don't update or change this configuration.
