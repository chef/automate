package shared

import "strings"

// commonCiphers are the SSL ciphers common to both our internal and
// external configurations. These uses OpenSSLs identifiers so they
// can be passed to software that takes an OpenSSL cipher list.
//
// This list was generated using the "intermediate" profile in Mozilla's SSL
// Configuration Generator
//
// https://mozilla.github.io/server-side-tls/ssl-config-generator/

var commonCiphers = []string{
	"ECDHE-ECDSA-AES256-GCM-SHA384",
	"ECDHE-RSA-AES256-GCM-SHA384",
	"ECDHE-ECDSA-CHACHA20-POLY1305",
	"ECDHE-RSA-CHACHA20-POLY1305",
	"ECDHE-ECDSA-AES128-GCM-SHA256",
	"ECDHE-RSA-AES128-GCM-SHA256",
	"DHE-RSA-AES128-GCM-SHA256",
	"DHE-RSA-AES256-GCM-SHA384",
}

// commonExcludes are exclude directives to be included in SSL cipher
// suite lists in the OpenSSL cipher list format. Based on our default
// cipher list, these should not typically be strictly necessary, but
// we include them to protect against future changes.
var commonExcludes = []string{
	"!aNULL",
	"!eNULL",
	"!EXPORT",
}

// InternalCipherSuites should be the default for ssl_ciphers for any
// processes that only handle internal requests. Note that these
// ciphers are not used for Golang services.
var InternalCipherSuite = toCipherString(append(commonCiphers, commonExcludes...))

// ExternalCipherSuite should be the default for ssl_ciphers for any
// processes that handle external requests. Note that these ciphers are
// not used for Golang services.
var ExternalCipherSuite = toCipherString(append(
	append(commonCiphers, commonExcludes...),
	// Added to support AWS "classic" ELB
	"AES256-GCM-SHA384"))

func toCipherString(l []string) string {
	return strings.Join(l, ":")
}
