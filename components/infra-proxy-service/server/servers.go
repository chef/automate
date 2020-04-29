package server

import (
	"context"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"

	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

// CreateServer creates a new server
func (s *Server) CreateServer(ctx context.Context, req *request.CreateServer) (*response.CreateServer, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	err := validation.New(validation.Options{
		Target:          "server",
		Request:         *req,
		RequiredDefault: false,

		Rules: validation.Rules{
			"Name": []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	server, err := s.service.Storage.StoreServer(ctx, req.Id, req.Name, req.Fqdn, req.IpAddress)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}

	return &response.CreateServer{
		Server: fromStorageServer(server),
	}, nil
}

// GetServers returns a list of servers from the db
func (s *Server) GetServers(ctx context.Context, req *request.GetServers) (*response.GetServers, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	serversList, err := s.service.Storage.GetServers(ctx)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}

	return &response.GetServers{
		Servers: fromStorageToListServers(serversList),
	}, nil
}

// GetServer takes an ID and returns a server object
func (s *Server) GetServer(ctx context.Context, req *request.GetServer) (*response.GetServer, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Id == "" {
		s.service.Logger.Debug("incomplete create server request: missing server ID")
		return nil, status.Error(codes.InvalidArgument, "must supply server ID")
	}

	server, err := s.service.Storage.GetServer(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}

	return &response.GetServer{
		Server: fromStorageServer(server),
	}, nil
}

// DeleteServer deletes a server from the db
func (s *Server) DeleteServer(ctx context.Context, req *request.DeleteServer) (*response.DeleteServer, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Id == "" {
		s.service.Logger.Debug("incomplete create server request: missing server ID")
		return nil, status.Error(codes.InvalidArgument, "must supply server ID")
	}

	server, err := s.service.Storage.DeleteServer(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}

	return &response.DeleteServer{
		Server: fromStorageServer(server),
	}, nil
}

// UpdateServer updates a server in the db via post
func (s *Server) UpdateServer(ctx context.Context, req *request.UpdateServer) (*response.UpdateServer, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Id == "" {
		s.service.Logger.Debug("incomplete update server request: missing server ID")
		return nil, status.Error(codes.InvalidArgument, "must supply server ID")
	}
	if req.Name == "" {
		s.service.Logger.Debug("incomplete update server request: missing server name")
		return nil, status.Error(codes.InvalidArgument, "must supply server name")
	}
	if req.Fqdn == "" {
		s.service.Logger.Debug("incomplete update server request: missing server fqdn")
		return nil, status.Error(codes.InvalidArgument, "must supply server fqdn")
	}
	if req.IpAddress == "" {
		s.service.Logger.Debug("incomplete update server request: missing server IP address")
		return nil, status.Error(codes.InvalidArgument, "must supply server IP address")
	}

	server, err := s.service.Storage.EditServer(ctx, req.Id, req.Name, req.Fqdn, req.IpAddress)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}

	return &response.UpdateServer{
		Server: fromStorageServer(server),
	}, nil
}

// Create a response.Server from a storage.Server
func fromStorageServer(s storage.Server) *response.Server {
	return &response.Server{
		Id:        s.ID,
		Name:      s.Name,
		Fqdn:      s.Fqdn,
		IpAddress: s.IPAddress,
		OrgsCount: s.OrgsCount,
	}
}

// Create a response.ServersList from an array of storage.Server
func fromStorageToListServers(sl []storage.Server) []*response.Server {
	tl := make([]*response.Server, len(sl))

	for i, server := range sl {
		tl[i] = fromStorageServer(server)
	}

	return tl
}
