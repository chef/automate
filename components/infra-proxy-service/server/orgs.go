package server

import (
	"context"

	"github.com/chef/automate/api/external/common/query"
	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/validation"
)

// CreateOrg creates a new org
func (s *Server) CreateOrg(ctx context.Context, req *request.CreateOrg) (*response.CreateOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "org",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	newSecret := &secrets.Secret{
		Name: "infra-proxy-service-admin-key",
		Type: "chef-server",
		Data: []*query.Kv{
			{Key: "key", Value: req.AdminKey},
		},
	}

	secretID, err := s.service.Secrets.Create(ctx, newSecret)
	if err != nil {
		return nil, err
	}

	org, err := s.service.Storage.StoreOrg(ctx, req.Id, req.Name, req.AdminUser, secretID.GetId(), req.ServerId, req.Projects)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "org")
	}

	return &response.CreateOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// GetOrgs returns a list of orgs from the db
func (s *Server) GetOrgs(ctx context.Context, req *request.GetOrgs) (*response.GetOrgs, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "org",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	orgsList, err := s.service.Storage.GetOrgs(ctx, req.ServerId)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "org")
	}

	return &response.GetOrgs{
		Orgs: fromStorageToListOrgs(orgsList),
	}, nil
}

// GetOrg takes an ID and returns an org object
func (s *Server) GetOrg(ctx context.Context, req *request.GetOrg) (*response.GetOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "org",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	org, err := s.service.Storage.GetOrg(ctx, req.Id, req.ServerId)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "org")
	}

	return &response.GetOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// DeleteOrg deletes an org from the db
func (s *Server) DeleteOrg(ctx context.Context, req *request.DeleteOrg) (*response.DeleteOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "org",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	org, err := s.service.Storage.DeleteOrg(ctx, req.Id, req.ServerId)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "org")
	}

	_, err = s.service.Secrets.Delete(ctx, &secrets.Id{Id: org.CredentialID})
	if err != nil {
		return nil, err
	}

	return &response.DeleteOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// UpdateOrg updates an org in the db via PUT
func (s *Server) UpdateOrg(ctx context.Context, req *request.UpdateOrg) (*response.UpdateOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "org",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	org, err := s.service.Storage.EditOrg(ctx, req.Id, req.Name, req.AdminUser, req.ServerId, req.Projects)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "org")
	}

	return &response.UpdateOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// ResetOrgAdminKey updates the org admin key via PUT
func (s *Server) ResetOrgAdminKey(ctx context.Context, req *request.ResetOrgAdminKey) (*response.ResetOrgAdminKey, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// Validate all request fields are required
	err := validation.New(validation.Options{
		Target:          "org",
		Request:         *req,
		RequiredDefault: true,
	}).Validate()

	if err != nil {
		return nil, err
	}

	org, err := s.service.Storage.GetOrg(ctx, req.Id, req.ServerId)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "org")
	}

	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: org.CredentialID})
	if err != nil {
		return nil, err
	}

	newSecret := &secrets.Secret{
		Id:   secret.GetId(),
		Name: "infra-proxy-service-admin-key",
		Type: "chef-server",
		Data: []*query.Kv{
			{Key: "key", Value: req.AdminKey},
		},
	}

	_, err = s.service.Secrets.Update(ctx, newSecret)
	if err != nil {
		return nil, err
	}

	org, err = s.service.Storage.TouchOrg(ctx, req.Id, req.ServerId)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "org")
	}

	return &response.ResetOrgAdminKey{
		Org: fromStorageOrg(org),
	}, nil
}

//GetInfraServerOrgs: Fetches the list of automate infra server organisations from the chef server and save it into the automate back end DB
func (s *Server) GetInfraServerOrgs(ctx context.Context, req *request.GetInfraServerOrgs) (*response.GetInfraServerOrgs, error) {
	// // TODO:Remove store webui key part: START
	// _, err := s.service.Secrets.Delete(ctx, &secrets.Id{Id: "1e2954bf-5021-46b0-8302-f8dbb85e2c0a"})
	// if err != nil {
	// 	return nil, err
	// }
	// 	webuiKey := ""
	// newSecret := &secrets.Secret{
	// 	Name: "infra-proxy-service-webui-key",
	// 	Type: "chef-server",
	// 	Data: []*query.Kv{
	// 		{Key: "key", Value: webuiKey},
	// 	},
	// }

	// 	credentialID, err := s.service.Secrets.Create(ctx, newSecret)
	// 	if err != nil {
	// 		return nil, err
	// 	}
	// 	fmt.Println("#########credentialID#########", credentialID)
	// TODO:Remove store webui key part: END

	// Get the credential ID from servers table
	server, err := s.service.Storage.GetServer(ctx, req.ServerId)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "server")
	}
	// Get web ui key from secrets service
	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: server.CredentialID})
	if err != nil {
		return nil, err
	}
	// Get organization list from chef server
	c, err := s.createChefServerClient(ctx, req.ServerId, GetWebuiKeyFrom(secret), "pivotal", true)
	if err != nil {
		return nil, err
	}
	orgsList, err := c.client.Organizations.List()
	if err != nil {
		return nil, ParseAPIError(err)
	}

	// Save organisations in backend DB
	orgs := []storage.Org{}
	for key := range orgsList {
		org, err := s.service.Storage.StoreOrg(ctx, key, key, "pivotal", "", req.ServerId, nil)
		if err != nil {
			return nil, service.ParseStorageError(err, *req, "org")
		}
		orgs = append(orgs, org)
	}

	return &response.GetInfraServerOrgs{
		Orgs: fromStorageToListOrgs(orgs),
	}, nil
}

// Create a response.Org from a storage.Org
func fromStorageOrg(s storage.Org) *response.Org {
	return &response.Org{
		Id:           s.ID,
		Name:         s.Name,
		AdminUser:    s.AdminUser,
		CredentialId: s.CredentialID,
		ServerId:     s.ServerID,
		Projects:     s.Projects,
	}
}

// Create a response.OrgsList from an array of storage.Org
func fromStorageToListOrgs(sl []storage.Org) []*response.Org {
	tl := make([]*response.Org, len(sl))

	for i, org := range sl {
		tl[i] = fromStorageOrg(org)
	}

	return tl
}
