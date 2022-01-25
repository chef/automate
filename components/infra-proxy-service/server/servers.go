package server

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"

	"github.com/chef/automate/api/external/common/query"
	secrets "github.com/chef/automate/api/external/secrets"
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
			"Id":       []string{"required"},
			"Name":     []string{"required"},
			"WebuiKey": []string{"required"},
		},
	}).Validate()

	if err != nil {
		return nil, err
	}

	if req.Fqdn == "" && req.IpAddress == "" {
		return nil, errors.Wrap(err, "FQDN or IP required to add the server.")
	}

	// validate the new webui key
	validateWebuiKey := request.ValidateWebuiKey{
		Id:       req.Id,
		Fqdn:     req.Fqdn,
		WebuiKey: req.WebuiKey,
	}

	res, err := s.ValidateWebuiKey(ctx, &validateWebuiKey)

	if err != nil {
		return nil, err
	}

	if !res.Valid {
		return nil, errors.New(res.Error)
	}

	newSecret := &secrets.Secret{
		Name: "infra-proxy-service-webui-key",
		Type: "chef-server",
		Data: []*query.Kv{
			{Key: "key", Value: req.WebuiKey},
		},
	}
	credential, err := s.service.Secrets.Create(ctx, newSecret)
	if err != nil {
		return nil, err
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

	server, err := s.service.Storage.StoreServer(ctx, req.Id, req.Name, req.Fqdn, req.IpAddress, credential.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}

	return &response.CreateServer{
		Server: fromStorageServer(server),
	}, nil
}

// ValidateWebuiKey validate the webui key
func (s *Server) ValidateWebuiKey(ctx context.Context, req *request.ValidateWebuiKey) (*response.ValidateWebuiKey, error) {

	webuiKey := req.WebuiKey
	if req.WebuiKey == "" {
		server, err := s.service.Storage.GetServer(ctx, req.Id)
		if err != nil {
			return nil, service.ParseStorageError(err, *req, "server")
		}

		if server.CredentialID == "" {
			return &response.ValidateWebuiKey{
				Valid: false,
				Error: "Webui key is not available for this server.",
			}, nil
		}

		// Get web ui key from secrets service
		secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: server.CredentialID})
		if err != nil {
			return nil, err
		}

		webuiKey = GetAdminKeyFrom(secret)
	}

	c, err := s.createCSClientWithFqdn(ctx, req.Fqdn, webuiKey, "pivotal", true)
	if err != nil {
		return &response.ValidateWebuiKey{
			Valid: false,
			Error: err.Error(),
		}, nil
	}
	_, err = c.client.License.Get()
	if err != nil {
		return &response.ValidateWebuiKey{
			Valid: false,
			Error: err.Error(),
		}, nil
	}

	return &response.ValidateWebuiKey{
		Valid: true,
		Error: "",
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
	migration, err := s.service.Migration.GetActiveMigration(ctx, req.Id)
	res1 := &response.GetServer{
		Server: fromStorageServerWithMigrationDetails(server, migration),
	}
	return res1, nil
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

// UpdateWebuiKey updates the webui key
func (s *Server) UpdateWebuiKey(ctx context.Context, req *request.UpdateWebuiKey) (*response.UpdateWebuiKey, error) {

	// validate the new webui key
	server, err := s.service.Storage.GetServer(ctx, req.Id)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}
	validateWebuiKeyReq := request.ValidateWebuiKey{
		Id:       req.Id,
		Fqdn:     server.Fqdn,
		WebuiKey: req.WebuiKey,
	}

	res, err := s.ValidateWebuiKey(ctx, &validateWebuiKeyReq)
	if err != nil {
		return nil, err
	}

	if !res.Valid {
		return nil, errors.New(res.Error)
	}

	newSecret := &secrets.Secret{
		Name: "infra-proxy-service-webui-key",
		Type: "chef-server",
		Data: []*query.Kv{
			{Key: "key", Value: req.WebuiKey},
		},
	}

	//If server does not have web ui key then create secret and save the credential id into the DB
	if server.CredentialID == "" {
		credential, err := s.service.Secrets.Create(ctx, newSecret)
		if err != nil {
			return nil, err
		}
		_, err = s.service.Storage.EditServerWebuiKey(ctx, req.Id, credential.Id)
		if err != nil {
			return nil, service.ParseStorageError(err, *req, "server")
		}
		return &response.UpdateWebuiKey{}, nil
	}

	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: server.CredentialID})
	if err != nil {
		return nil, err
	}
	newSecret.Id = secret.GetId()

	_, err = s.service.Secrets.Update(ctx, newSecret)
	if err != nil {
		return nil, err
	}

	return &response.UpdateWebuiKey{}, nil
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

// Create a response.Server from a storage.Server with migration
func fromStorageServerWithMigrationDetails(s storage.Server, m storage.ActiveMigration) *response.Server {
	fmt.Println("testing the migration", m, "--------------------------")
	return &response.Server{
		Id:              s.ID,
		Name:            s.Name,
		Fqdn:            s.Fqdn,
		IpAddress:       s.IPAddress,
		OrgsCount:       s.OrgsCount,
		MigrationId:     m.MigrationId,
		MigrationStatus: m.MigrationType,
	}
}
