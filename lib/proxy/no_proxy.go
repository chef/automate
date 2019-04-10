package proxy

// DefaultNoProxyEntries are IP addresses,
var DefaultNoProxyEntries = []string{
	// IPv4 and IPv6 localhost entries
	//
	// CIDR notation is supported by Go's standard library, but
	// won't necessarily work with other tools
	"127.0.0.1/8",
	"127.0.0.1",
	"localhost",
	"::1/128",
	"::1",
	"localhost6",
	// When used as a client address INADDR_ANY is translated to
	// localhost by the Linux kernel. Most uses of this are just
	// in test code, but it is included here for completeness.
	"0.0.0.0",
	"::0",
}
