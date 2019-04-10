package rest

import (
	"fmt"
	"net/http"

	"context"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	log "github.com/sirupsen/logrus"

	gw "github.com/chef/automate/api/interservice/ingest"
	"github.com/chef/automate/components/ingest-service/serveropts"
)

// Spawn starts a gRPC REST Client listening on the provided port and host,
// it will forward the requests to the provided (gRPC Server) endpoint
//
// You can start a client in a goroutine to work independently like:
// ```
// go rest.Spawn("localhost", "2194", "my-grpc-server.example.com:2191")
// ```
//
// Maybe even spawn multiple clients
func Spawn(endpoint string, conf *serveropts.Opts) error {
	uri := fmt.Sprintf("%s:%d", conf.Host, conf.RestPort)
	log.WithFields(log.Fields{
		"uri":      uri,
		"endpoint": endpoint,
	}).Info("Starting gRPC REST Client")

	ctx := context.Background()
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	mux := runtime.NewServeMux(
		runtime.WithMarshalerOption(runtime.MIMEWildcard, &runtime.JSONPb{OrigName: true, EmitDefaults: true}))
	opts := conf.ConnFactory.DialOptions("ingest-service")

	err := gw.RegisterChefIngesterHandlerFromEndpoint(ctx, mux, endpoint, opts)
	if err != nil {
		log.Error(err)
		return err
	}

	err = gw.RegisterJobSchedulerHandlerFromEndpoint(ctx, mux, endpoint, opts)
	if err != nil {
		log.Error(err)
		return err
	}

	return http.ListenAndServe(uri, mux)
}
