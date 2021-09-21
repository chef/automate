package server

import (
	"context"
	"encoding/json"
	"io/ioutil"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/validation"
	"github.com/pkg/errors"
)

func (s *Server) SetAuthenticator(statusChecker StatusChecker) {
	s.infraServerStatusChecker = statusChecker
}

// CreateServer creates a new server
func (s *Server) CreateServer(ctx context.Context, req *request.CreateServer) (*response.CreateServer, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate Name and ID are required.
	err := validation.New(validation.Options{
		Target:  "server",
		Request: *req,
		Rules: validation.Rules{
			"Id":   []string{"required"},
			"Name": []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, err
	}

	if req.Fqdn == "" && req.IpAddress == "" {
		return nil, errors.Wrap(err, "FQDN or IP required to add the server.")
	}

	// commenting this code because sometimes chef-services are down that time we can't able to check the server status and not able to add or update the server.
	/*
		serverHost := req.GetFqdn()
		if serverHost == "" {
			serverHost = req.GetIpAddress()
		}

		_, err = s.infraServerStatusChecker.GetInfraServerStatus(serverHost)
		if err != nil {
			return nil, err
		}
	*/

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

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "server",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
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

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "server",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
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

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:  "server",
		Request: *req,
		Rules: validation.Rules{
			"Id":   []string{"required"},
			"Name": []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, err
	}

	if req.Fqdn == "" && req.IpAddress == "" {
		return nil, errors.New("FQDN or IP required to update the server.")
	}

	// commenting this code because sometimes chef-services are down that time we can't able to check the server status and not able to add or update the server.
	/*
		serverHost := req.GetFqdn()
		if serverHost == "" {
			serverHost = req.GetIpAddress()
		}

		_, err = s.infraServerStatusChecker.GetInfraServerStatus(serverHost)
		if err != nil {
			return nil, err
		}
	*/

	server, err := s.service.Storage.EditServer(ctx, req.Id, req.Name, req.Fqdn, req.IpAddress)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}

	return &response.UpdateServer{
		Server: fromStorageServer(server),
	}, nil
}

// GetServerStatus get the status of server
func (s *Server) GetServerStatus(ctx context.Context, req *request.GetServerStatus) (*response.GetServerStatus, error) {
	_, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:  "server",
		Request: *req,
		Rules: validation.Rules{
			"Id":   []string{"required"},
			"Name": []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, err
	}

	if req.Fqdn == "" && req.IpAddress == "" {
		return nil, errors.New("FQDN or IP required to update the server.")
	}

	serverHost := req.GetFqdn()
	if serverHost == "" {
		serverHost = req.GetIpAddress()
	}

	res, err := s.infraServerStatusChecker.GetInfraServerStatus(serverHost)
	if err != nil {
		return nil, err
	}

	// read all response body
	data, err := ioutil.ReadAll(res.Body)
	if err != nil {
		return nil, err
	}
	// close response body
	_ = res.Body.Close()

	statusRes := &response.GetServerStatus{}
	err = json.Unmarshal(data, statusRes)
	if err != nil {
		return nil, err
	}

	return statusRes, nil
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
