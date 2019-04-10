package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/pkg/errors"
	"google.golang.org/grpc"
	healthpb "google.golang.org/grpc/health/grpc_health_v1"

	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

// exit code interpretation: see https://www.habitat.sh/docs/reference/#health_check

const (
	ok       = iota // nolint // 0,
	warning         // nolint // 1, ...
	critical        // nolint
	unknown         // nolint
	debug           // nolint // >= 4
)

// This implements a terribly simple GRPC client binary, asking a service's
// grpc_health_v1 Check method, and translating that into an exit code that
// fits into what a habitat health_check hook expects.
func main() {
	if len(os.Args) < 7 {
		printUsageExit()
	}

	host, port, svc := os.Args[1], os.Args[2], os.Args[3]
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	tlsCfg := certs.TLSConfig{
		CertPath:       os.Args[4],
		KeyPath:        os.Args[5],
		RootCACertPath: os.Args[6],
	}
	tlsCfg.FixupRelativeTLSPaths(os.Args[0]) // fix paths relative to executable
	certs, err := tlsCfg.ReadCerts()
	if err != nil {
		criticalExit(err)
	}

	// Disable HTTP proxying for the host we are trying to connect
	// to. Currently we assume that any user-provided proxies will
	// only be used for external services.
	err = os.Setenv("NO_PROXY", host)
	if err != nil {
		criticalExit(errors.Wrap(err, "could not override NO_PROXY environment variable"))
	}

	connFactory := secureconn.NewFactory(*certs)
	conn, err := connFactory.DialContext(ctx, svc, host+":"+port, grpc.WithBlock())
	if err != nil {
		criticalExit(err)
	}
	defer conn.Close() // nolint: errcheck

	cl := healthpb.NewHealthClient(conn)
	status, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
	if err != nil {
		criticalExit(err)
	}
	if s := status.GetStatus(); s != healthpb.HealthCheckResponse_SERVING {
		criticalExit(errors.Errorf("status not serving: %q", s))
	}

	// Note 2018/04/19 (sr): we've found this binary to cause a log line,
	//   transport: http2Server.HandleStreams failed to read frame: read tcp
	//   10.0.2.15:10130->10.0.2.15:44674: read: connection reset by peer
	// on every run, where its sibling,
	//   components/local-user-service/cmd/users-health
	// didn't.
	// Trying to figure out the difference, none of the obvious differences, like
	// cancelling request contexts, and nothing that might seem related and
	// reasonable, like closing the client connection, did make a difference.
	// Calling the other method, however, made the log line go away. The only
	// difference there is that the other method does more things than `Check()`.
	//
	// And as it turns out, timing seems to be the issue, as the following makes
	// the logged line disappear as well:
	time.Sleep(100 * time.Millisecond)
}

func printUsageExit() {
	fmt.Fprintf(os.Stderr, "usage: %s host port service-name /path/to/service.cert "+ // nolint: gas
		"/path/to/service.key /path/to/root-ca.crt\n", os.Args[0])
	os.Exit(unknown)
}

func criticalExit(err error) {
	fmt.Fprintf(os.Stderr, "critical: %s\n", err.Error()) // nolint: gas
	os.Exit(critical)
}
