package server

import (
	"context"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
)

//GetAutomateInfraServerUsersList: Fetches the list of automate infra server users from the DB
func (s *Server) GetAutomateInfraServerUsersList(ctx context.Context, req *request.AutomateInfraServerUsers) (*response.AutomateInfraServerUsers, error) {
	// Check server exists in automate or not
	err := s.isServerExistInAutomate(ctx, req.ServerId)
	if err != nil {
		return nil, service.ParseStorageError(err, req.ServerId, "server")
	}

	usersList, err := s.service.Storage.GetAutomateInfraServerUsers(ctx, req.ServerId)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "user")
	}

	return &response.AutomateInfraServerUsers{
		Users: fromStorageToListAutomateInfraServerUsers(usersList),
	}, nil
}

//GetAutomateInfraOrgUsersList: Fetches the list of automate infra org users from the DB
func (s *Server) GetAutomateInfraOrgUsersList(ctx context.Context, req *request.AutomateInfraOrgUsers) (*response.AutomateInfraOrgUsers, error) {

	usersList, err := s.service.Storage.GetAutomateInfraOrgUsers(ctx, req.ServerId, req.OrgId)
	if err != nil {
		return nil, service.ParseStorageError(err, *req, "user")
	}

	return &response.AutomateInfraOrgUsers{
		Users: fromStorageToListAutomateInfraOrgUsers(usersList),
	}, nil
}

// fromStorageAutomateInfraServerUser: Create a response.AutomateInfraServerUsersListItem from a storage.User
func fromStorageAutomateInfraServerUser(u storage.User) *response.AutomateInfraServerUsersListItem {
	return &response.AutomateInfraServerUsersListItem{
		Id:                  u.ID,
		ServerId:            u.ServerID,
		InfraServerUsername: u.InfraServerUsername,
		Connector:           u.Connector,
		AutomateUserId:      u.AutomateUserID,
		IsServerAdmin:       u.IsServerAdmin,
	}
}

//fromStorageToListAutomateInfraServerUsers: Create a response.AutomateInfraServerUsersListItem from an array of storage.User
func fromStorageToListAutomateInfraServerUsers(ul []storage.User) []*response.AutomateInfraServerUsersListItem {
	tl := make([]*response.AutomateInfraServerUsersListItem, len(ul))

	for i, user := range ul {
		tl[i] = fromStorageAutomateInfraServerUser(user)
	}

	return tl
}

//isServerExistInAutomate: Check whether server exist in automate or not
func (s *Server) isServerExistInAutomate(ctx context.Context, serverId string) error {
	_, err := s.service.Storage.GetServer(ctx, serverId)
	if err != nil {
		return err
	}
	return nil
}

// fromStorageAutomateInfraServerUser: Create a response.AutomateInfraOrgUsersListItem from a storage.User
func fromStorageAutomateInfraOrgUser(u storage.OrgUser) *response.AutomateInfraOrgUsersListItem {
	return &response.AutomateInfraOrgUsersListItem{
		UserId:              int32(u.UserID),
		ServerId:            u.ServerID,
		OrgId:               u.OrgID,
		InfraServerUsername: u.InfraServerUsername,
		FirstName:           u.FirstName,
		LastName:            u.LastName,
		EmailId:             u.EmailID,
		MiddleName:          u.MiddleName,
		DisplayName:         u.DisplayName,
		Connector:           u.Connector,
		AutomateUserId:      u.AutomateUserID,
		IsAdmin:             u.IsAdmin,
	}
}

//fromStorageToListAutomateInfraOrgUsers: Create a response.AutomateInfraServerUsersListItem from an array of storage.User
func fromStorageToListAutomateInfraOrgUsers(ul []storage.OrgUser) []*response.AutomateInfraOrgUsersListItem {
	tl := make([]*response.AutomateInfraOrgUsersListItem, len(ul))

	for i, user := range ul {
		tl[i] = fromStorageAutomateInfraOrgUser(user)
	}

	return tl
}
