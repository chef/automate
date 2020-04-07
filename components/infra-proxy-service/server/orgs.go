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
	uuid "github.com/chef/automate/lib/uuid4"
)

// CreateOrg creates a new org
func (s *Server) CreateOrg(ctx context.Context, req *request.CreateOrg) (*response.CreateOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Name == "" {
		s.service.Logger.Debug("incomplete create org request: missing org name")
		return nil, status.Error(codes.InvalidArgument, "must supply org name")
	}

	if req.AdminUser == "" {
		s.service.Logger.Debug("incomplete create org request: missing org admin_user")
		return nil, status.Error(codes.InvalidArgument, "must supply org admin_user")
	}

	if req.AdminKey == "" {
		s.service.Logger.Debug("incomplete create org request: missing org admin_key")
		return nil, status.Error(codes.InvalidArgument, "must supply org admin_key")
	}

	if req.ServerId == "" {
		s.service.Logger.Debug("incomplete create org request: missing server id")
		return nil, status.Error(codes.InvalidArgument, "must supply server id")
	}

	newSecret := &secrets.Secret{
		Name: "infra-proxy-service-admin-key",
		Type: "ssh",
		Data: []*query.Kv{
			{Key: "username", Value: req.AdminUser},
			{Key: "key", Value: req.AdminKey},
		},
	}

	secretID, err := s.service.Secrets.Create(ctx, newSecret)
	if err != nil {
		return nil, err
	}

	org, err := s.service.Storage.StoreOrg(ctx, req.Name, req.AdminUser, secretID.GetId(), req.ServerId, req.Projects)
	if err != nil {
		return nil, service.ParseStorageError(err, req, "org")
	}

	return &response.CreateOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// GetOrgs returns a list of orgs from the db
func (s *Server) GetOrgs(ctx context.Context, req *request.GetOrgs) (*response.GetOrgs, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	serverID, err := uuid.FromString(req.ServerId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org server_id: %s", err.Error())
	}

	orgsList, err := s.service.Storage.GetOrgs(ctx, serverID)
	if err != nil {
		return nil, service.ParseStorageError(err, req, "org")
	}

	return &response.GetOrgs{
		Orgs: fromStorageToListOrgs(orgsList),
	}, nil
}

// GetOrg takes an ID and returns an org object
func (s *Server) GetOrg(ctx context.Context, req *request.GetOrg) (*response.GetOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	UUID, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	org, err := s.service.Storage.GetOrg(ctx, UUID)
	if err != nil {
		return nil, service.ParseStorageError(err, req, "org")
	}

	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: org.AdminKey})
	if err != nil {
		return nil, err
	}

	return &response.GetOrg{
		Org: &response.Org{
			Id:        org.ID.String(),
			Name:      org.Name,
			AdminUser: org.AdminUser,
			AdminKey:  GetOrgAdminKeyFrom(secret),
			ServerId:  org.ServerId,
			Projects:  org.Projects,
		},
	}, nil
}

// GetOrgByName takes an org name, server_id and returns an org object
func (s *Server) GetOrgByName(ctx context.Context, req *request.GetOrgByName) (*response.GetOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Name == "" {
		s.service.Logger.Debug("incomplete server request: missing org name")
		return nil, status.Error(codes.InvalidArgument, "must supply org name")
	}

	serverID, err := uuid.FromString(req.ServerId)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org server_id: %s", err.Error())
	}

	org, err := s.service.Storage.GetOrgByName(ctx, req.Name, serverID)
	if err != nil {
		return nil, service.ParseStorageError(err, req, "org")
	}

	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: org.AdminKey})
	if err != nil {
		return nil, err
	}

	return &response.GetOrg{
		Org: &response.Org{
			Id:        org.ID.String(),
			Name:      org.Name,
			AdminUser: org.AdminUser,
			AdminKey:  GetOrgAdminKeyFrom(secret),
			ServerId:  org.ServerId,
		},
	}, nil
}

// DeleteOrg deletes an org from the db
func (s *Server) DeleteOrg(ctx context.Context, req *request.DeleteOrg) (*response.DeleteOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	UUID, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	org, err := s.service.Storage.DeleteOrg(ctx, UUID)
	if err != nil {
		return nil, service.ParseStorageError(err, req, "org")
	}

	_, err = s.service.Secrets.Delete(ctx, &secrets.Id{Id: org.AdminKey})
	if err != nil {
		return nil, err
	}

	return &response.DeleteOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// UpdateOrg updates an org in the db via post
func (s *Server) UpdateOrg(ctx context.Context, req *request.UpdateOrg) (*response.UpdateOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Id == "" {
		s.service.Logger.Debug("incomplete update org request: missing org id")
		return nil, status.Error(codes.InvalidArgument, "must supply org id")
	}
	if req.Name == "" {
		s.service.Logger.Debug("incomplete update org request: missing org name")
		return nil, status.Error(codes.InvalidArgument, "must supply org name")
	}
	if req.AdminUser == "" {
		s.service.Logger.Debug("incomplete update org request: missing org admin_user")
		return nil, status.Error(codes.InvalidArgument, "must supply org admin_user")
	}
	if req.AdminKey == "" {
		s.service.Logger.Debug("incomplete update org request: missing org admin_key")
		return nil, status.Error(codes.InvalidArgument, "must supply org admin_key")
	}
	if req.ServerId == "" {
		s.service.Logger.Debug("incomplete update org request: missing server id")
		return nil, status.Error(codes.InvalidArgument, "must supply server id")
	}

	id, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "invalid org id: %s", err.Error())
	}

	oldOrg, err := s.service.Storage.GetOrg(ctx, id)
	if err != nil {
		return nil, service.ParseStorageError(err, req, "org")
	}

	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: oldOrg.AdminKey})
	if err != nil {
		return nil, err
	}

	rawAdminKey := req.AdminKey

	newSecret := &secrets.Secret{
		Id:   secret.GetId(),
		Name: "infra-proxy-service-admin-key",
		Type: "ssh",
		Data: []*query.Kv{
			{Key: "username", Value: req.AdminUser},
			{Key: "key", Value: rawAdminKey},
		},
	}

	_, err = s.service.Secrets.Update(ctx, newSecret)
	if err != nil {
		return nil, err
	}

	orgStruct := storage.Org{
		ID:        id,
		Name:      req.Name,
		AdminUser: req.AdminUser,
		AdminKey:  oldOrg.AdminKey,
		ServerId:  req.ServerId,
		Projects:  req.Projects,
	}

	org, err := s.service.Storage.EditOrg(ctx, orgStruct)
	if err != nil {
		return nil, service.ParseStorageError(err, req, "org")
	}

	return &response.UpdateOrg{
		Org: &response.Org{
			Id:        org.ID.String(),
			Name:      org.Name,
			AdminUser: org.AdminUser,
			AdminKey:  rawAdminKey,
			ServerId:  org.ServerId,
			Projects:  org.Projects,
		},
	}, nil
}

// Create a response.Org from a storage.Org
func fromStorageOrg(s storage.Org) *response.Org {
	return &response.Org{
		Id:        s.ID.String(),
		Name:      s.Name,
		AdminUser: s.AdminUser,
		AdminKey:  s.AdminKey,
		ServerId:  s.ServerId,
		Projects:  s.Projects,
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
