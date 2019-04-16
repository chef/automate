# Event Ingest Service

This service is the primary ingress event handler for external events in Chef
Automate 2.0.

## Geting Started

This component uses the habitat studio development environment. See
[dev-docs/DEV_ENVIRONMENT](https://github.com/chef/automate/blob/master/dev-docs/DEV_ENVIRONMENT.md)
to get started.

## Design Goals

- small memory footprint
- fast event processing
- flexible pipelines

Automate 1.x ingestion pipeline was slow and resource hungry. Many
users complained about its memory requirements and it was the cause of
many operational issues. Our goal is to handle Chef Automate's largest
users efficiently.

## Event types

At this point the service is intended to handle the following event types:

- Chef run events


## Event Pipelines

We are going to use goroutines and channels for our pipeline implementation. The following resources explain the concepts:

 - [Google I/O 2012 - Go Concurrency Patterns](https://www.youtube.com/watch?v=f6kdp27TYZs)
 - [Go Concurrency Patterns: Pipelines and cancellation](https://blog.golang.org/pipelines)
 - [Advanced Go Concurrency Patterns](https://blog.golang.org/advanced-go-concurrency-patterns)

### Chef Run Pipeline

- Receive message
- Initialize elastic search mappings and aliases if they do not yet exist
- Break apart messages into respective indexes
- transform messages into expected elastic search document
- submit message to elasticsearch
- record errors

## Performance Test

To run our performance tests you have to first enter the habitat studio, build and start the service.

Then you can launch the tests with the helper method:

	[1][default:/src:0]# ingest_load [NODES] [CCR] [TIME]

Parameters:

* **NODES**: The number of nodes to mock. _[default:10]_
* **CCR**: The number of chef-client runs to mock. _[default:50]_
* **TIME**: The duration of the ingestion. (e.g. 2s, 2m, 2h) _[default:1m]_

To trigger an ingestion of 10 Nodes sending each 50 chef-client runs simultaneously for 1 minute:

	hab studio enter
	[1][default:/src:0]# setup_dev_environment && build && start
	[2][default:/src:0]# ingest_load 10 50 1m

## Development/Usage

```
git clone git@github.com:chef/automate/components/ingest-service.git $GOPATH/src/github.com/chef/automate/components/ingest-service
make install-dev
dep ensure
```

Now you can run the service via:

```
make run
```

Then you can test it with:
```
http -f POST localhost:2192/events/chef/run          < examples/chef_client_run.json
```

or

```
curl --data "@-" localhost:2192/events/chef/run          < examples/chef_client_run.json
```

## Regenerating protobuf endpoints

gRPC endpoints are generated from the protobuf definitions using grpc-gateway.
You will need the protocol compiler which is available here:
```
https://github.com/google/protobuf/releases
```
Then follow the installation instructions here, including installing the google :
```
https://github.com/grpc-ecosystem/grpc-gateway
```
Once these are in place run `make proto` to regenerate endpoints.


## Call the GRPC service

The following code snippet illustrates the use of the GRPC Service

```go
package main

import (
	log "github.com/sirupsen/logrus"
	pb "github.com/chef/automate/api/interservice/ingest"
	"golang.org/x/net/context"
	"google.golang.org/grpc"
)

func main() {
	serverAddr := "127.0.0.1:2191"
	conn, err := grpc.Dial(serverAddr, grpc.WithInsecure())
	if err != nil {
		log.Error(err)
	}

	defer conn.Close()

	client := pb.NewChefIngesterClient(conn)
	version, err := client.GetVersion(context.Background(), &pb.Empty{})
	if err != nil {
		log.Error(err)
	}
	log.Info(version)
}
```
