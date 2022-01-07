package server

import (
	"context"
	"errors"

	"github.com/chef/automate/api/external/common/query"
	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/validation"
	"github.com/go-chef/chef"
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

	// Check whether any migration is in progress or not
	if IsMigrationAlreadyRunning {
		return nil, errors.New("Migration is already in process")
	}

	// Get chef client
	client, err := s.getChefClient(ctx, req.ServerId)
	if err != nil {
		return nil, err
	}

	setMigrationStatus(true)
	defer setMigrationStatus(false)

	//Store the status in migration table as in progress
	migration, err := s.service.Storage.StoreMigration(ctx, req.ServerId, "org", "In Progress")
	if err != nil {
		return nil, err
	}

	// Get and save orgs in goroutine
	go s.getInfraServerOrgs(client, req.ServerId, migration)

	return &response.GetInfraServerOrgs{
		MigrationId: migration.ID,
	}, nil
}

func (s *Server) getInfraServerOrgs(c *ChefClient, serverId string, migration storage.Migration) {
	var migrationStatus string
	var totalSucceeded, totalSkipped, totalFailed int64

	defer func() {
		s.service.Storage.EditMigration(context.Background(), migration.ID, migration.TypeID, migrationStatus, totalSucceeded, totalSkipped, totalFailed)
	}()

	// Get organisation list from chef server
	orgsList, err := c.client.Organizations.List()
	if err != nil {
		migrationStatus = "Failed"
		return
	}

	// Save organisations in backend DB
	for key := range orgsList {
		_, err := s.service.Storage.StoreOrg(context.Background(), key, key, "", "", serverId, nil)
		if err != nil {
			totalFailed++
			continue
		}
		totalSucceeded++
	}
	migrationStatus = "Completed"
	return
}

//CreateInfraServerOrgs: Creates the organisations on chef server
func (s *Server) CreateInfraServerOrgs(ctx context.Context, req *request.CreateInfraServerOrgs) (*response.CreateInfraServerOrgs, error) {

	// Get chef client
	c, err := s.getChefClient(ctx, req.ServerId)
	if err != nil {
		return nil, err
	}

	org := chef.Organization{
		Name:     req.Name,
		FullName: req.FullName,
	}
	_, err = c.client.Organizations.Create(org)
	if err != nil {
		return nil, err
	}
	return &response.CreateInfraServerOrgs{}, nil
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
