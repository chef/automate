package integration_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
)


func TestClientsReturnsEmptyList(t *testing.T) {
	// rpc GetClients (request.Clients) returns (response.Clients)
	ctx := context.Background()
	req := request.Clients{}

	expected := new(response.Clients)
    res, err := infraProxy.GetClients(ctx, &req)
    fmt.Print(expected)
    fmt.Print(res)
    fmt.Print(err)
}