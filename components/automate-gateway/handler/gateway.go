//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package handler

import (
	"context"

	gp "github.com/golang/protobuf/ptypes/empty"

	pb "github.com/chef/automate/components/automate-gateway/api/gateway"
	"github.com/chef/automate/lib/version"
)

var SERVICE_NAME = "automate-gateway"

type Gateway struct{}

func NewGatewayServer() *Gateway {
	return new(Gateway)
}

// GetVersion returns the service version
func (m *Gateway) GetVersion(ctx context.Context, empty *gp.Empty) (*pb.Version, error) {
	return &pb.Version{
		Name:    SERVICE_NAME,
		Version: version.Version,
		Built:   version.BuildTime,
		Sha:     version.GitSHA,
	}, nil
}

// GetHealth returns the health of the service
func (m *Gateway) GetHealth(ctx context.Context, empty *gp.Empty) (*pb.Health, error) {
	// @afiune: Inspect the service and report its health
	return &pb.Health{
		Status: "ok",
	}, nil
}
