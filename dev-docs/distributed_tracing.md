# Distributed Tracing

Currently we use [Jaeger](http://jaegertracing.io/) as a distributed tracing tool which is an implementation of the [OpenTracing](http://opentracing.io/) standard.

## Adding Traces

Tracing Interceptors for gRPC have been added to the components. If you need to add an interceptor to a new component:
```
// components/SOME_SERVICE/pkg/server/grpc.go

import "github.com/chef/automate/lib/tracing"

// StartGRPC starts the gRPC server
func StartGRPC(ctx context.Context, config *Config) {
	// Setup our gRPC connection factory
	connFactory := secureconn.NewFactory(*config.ServiceCerts)

	// Create our listener channel

	// Register our API with a tracing interceptor
	grpcServer := connFactory.NewServer(tracing.GlobalServerInterceptor())

	// Remaining gRPC Setup
	srv := NewSERVICEServer(config)
	lc.RegisterSERVICEServer(grpcServer, srv)
	reflection.Register(grpcServer)
	grpcServer.Serve(listener)

	return
}
```

To add traces to your code:
```
// import tracing
import "github.com/chef/automate/lib/tracing"

// basic type for this example
type Service struct {}

// Some function we add a span to
func (s *Service) someFunction(string) error {
  span := tracing.StartSpan("some_function")
	defer span.Finish()

  // do something

  return nil
}

// Another function that needs tracing but has context
func (s *Service) Update(ctx context.Context, req *s.UpdateRequest) (*s.UpdateResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "update")
	defer span.Finish()

  // code

  // tracing span logs
  span.LogFields(
		spanlog.String("updated", req.Data),
		spanlog.Object("updated_data", req.Data),
	)
}
```

Some traces were added to licensing-control-service for additional examples.

## Setting Up Jaeger Agent & Collector

If you need distributed tracing in development you can install the [Jaeger All-In-One Docker](https://jaeger.readthedocs.io/en/latest/getting_started/) image in your A2 Vagrant instance. This includes the Jaeger Agent, Collector, and UI.

- Setup your A2 Vagrant Instance
- Install Docker:
  * `sudo apt-get update && sudo apt-get install docker-compose --yes`
- Run the Jaeger All-In-One Docker image:
  *
  ```
  docker run -d -e COLLECTOR_ZIPKIN_HTTP_PORT=9411 -p5775:5775/udp -p6831:6831/udp -p6832:6832/udp \
  -p5778:5778 -p16686:16686 -p14268:14268 -p9411:9411 jaegertracing/all-in-one:latest
  ```
- Access the Jaeger UI at: `http://automate-deployment.test:16686`
