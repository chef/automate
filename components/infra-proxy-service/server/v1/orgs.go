package v1

import (
	"context"

	uuid "github.com/chef/automate/lib/uuid4"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"

	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
)

// CreateOrg creates a new org
func (s *Server) CreateOrg(ctx context.Context, req *request.CreateOrg) (*response.CreateOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	if req.Name == "" {
		s.service.Logger.Debug("incomplete create org request: missing org name")
		return nil, status.Error(codes.InvalidArgument, "must supply sever name")
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
		s.service.Logger.Debug("incomplete create org request: missing org server id")
		return nil, status.Error(codes.InvalidArgument, "must supply org server id")
	}

	var org storage.Org
	var err error
	if org, err = s.service.Storage.StoreOrg(ctx, req.Name, req.AdminUser, req.AdminKey, req.ServerId); err != nil {
		if err == storage.ErrConflict {
			return nil, status.Errorf(codes.AlreadyExists, "org with name %q already exists", req.Name)
		}
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.CreateOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// GetOrgs returns a list of orgs from the db
func (s *Server) GetOrgs(ctx context.Context, req *request.GetOrgs) (*response.GetOrgs, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	orgsList, err := s.service.Storage.GetOrgs(ctx)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &response.GetOrgs{
		Orgs: fromStorageToListOrgs(orgsList),
	}, nil
}

// GetOrg takes an ID and returns a org object
func (s *Server) GetOrg(ctx context.Context, req *request.GetOrg) (*response.GetOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	UUID, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "invalid org id")
	}

	org, err := s.service.Storage.GetOrg(ctx, UUID)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "org")
	}

	return &response.GetOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// DeleteOrg deletes a org from the db
func (s *Server) DeleteOrg(ctx context.Context, req *request.DeleteOrg) (*response.DeleteOrg, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	UUID, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "invalid org id")
	}

	org, err := s.service.Storage.DeleteOrg(ctx, UUID)
	if err != nil {
		return nil, service.ParseStorageError(err, req.Id, "org")
	}

	if err != nil {
		s.service.Logger.Warnf("failed to purge subjects on org delete: %s", err.Error())
		return nil, status.Errorf(codes.Internal, "failed to purge org %q from policies: %s", org.ID, err.Error())
	}

	return &response.DeleteOrg{
		Org: fromStorageOrg(org),
	}, nil
}

// UpdateOrg updates a org in the db via post
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
		s.service.Logger.Debug("incomplete update org request: missing org server_id")
		return nil, status.Error(codes.InvalidArgument, "must supply org server_id")
	}

	id, err := uuid.FromString(req.Id)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, "invalid org id")
	}

	orgStruct := storage.Org{
		ID:        id,
		Name:      req.Name,
		AdminUser: req.AdminUser,
		AdminKey:  req.AdminKey,
		ServerId:  req.ServerId,
	}

	org, err := s.service.Storage.EditOrg(ctx, orgStruct)
	if err != nil {
		return nil, service.ParseStorageError(err, id, "org")
	}

	return &response.UpdateOrg{
		Org: fromStorageOrg(org),
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
