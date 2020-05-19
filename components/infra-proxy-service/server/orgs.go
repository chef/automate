package server

import (
	"context"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/external/common/query"
	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
)

// CreateOrg creates a new org
func (s *Server) CreateOrg(ctx context.Context, req *request.CreateOrg) (*response.CreateOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Id == "" {
		s.service.Logger.Debug("incomplete create org request: missing org ID")
		return nil, status.Error(codes.InvalidArgument, "must supply org ID")
	}

	if req.Name == "" {
		s.service.Logger.Debug("incomplete create org request: missing org name")
		return nil, status.Error(codes.InvalidArgument, "must supply org name")
	}

	if req.AdminUser == "" {
		s.service.Logger.Debug("incomplete create org request: missing org admin user")
		return nil, status.Error(codes.InvalidArgument, "must supply org admin user")
	}

	if req.AdminKey == "" {
		s.service.Logger.Debug("incomplete create org request: missing org admin key")
		return nil, status.Error(codes.InvalidArgument, "must supply org admin key")
	}

	if req.ServerId == "" {
		s.service.Logger.Debug("incomplete create org request: missing server ID")
		return nil, status.Error(codes.InvalidArgument, "must supply server ID")
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

	if req.ServerId == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply server ID")
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

	if req.Id == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply org ID")
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

	if req.Id == "" {
		return nil, status.Error(codes.InvalidArgument, "must supply org ID")
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

	if req.Id == "" {
		s.service.Logger.Debug("incomplete update org request: missing org ID")
		return nil, status.Error(codes.InvalidArgument, "must supply org ID")
	}
	if req.Name == "" {
		s.service.Logger.Debug("incomplete update org request: missing org name")
		return nil, status.Error(codes.InvalidArgument, "must supply org name")
	}
	if req.AdminUser == "" {
		s.service.Logger.Debug("incomplete update org request: missing org admin user")
		return nil, status.Error(codes.InvalidArgument, "must supply org admin user")
	}
	if req.ServerId == "" {
		s.service.Logger.Debug("incomplete update org request: missing server ID")
		return nil, status.Error(codes.InvalidArgument, "must supply server ID")
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
		Id:       org.ID,
		ServerId: org.ServerID,
		Status:   "success",
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
