package server

import (
	"context"
	"fmt"

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

//GetAutomateInfraServerUsersList: Fetches the list of automate infra server users from the DB
func (s *Server) GetAutomateInfraServerOrgs(ctx context.Context, req *request.AutomateInfraServerOrgs) (*response.AutomateInfraServerOrgs, error) {
	// Get organization list from chef server
	c, err := s.createChefServerClient(ctx, req.ServerId, req.WebuiKey, "pivotal")
	if err != nil {
		return nil, err
	}
	orgsList, err := c.client.Organizations.List()
	fmt.Println("###orgsList###", orgsList)

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

	return &response.AutomateInfraServerOrgs{
		Orgs: fromStorageToListAutomateInfraServerOrgs(orgs),
	}, nil
}

// fromStorageAutomateInfraServerUser: Create a response.AutomateInfraServerUsersListItem from a storage.User
func fromStorageAutomateInfraServerUser(u storage.User) *response.AutomateInfraServerUsersListItem {
	return &response.AutomateInfraServerUsersListItem{
		Id:                  u.ID,
		ServerId:            u.ServerID,
		InfraServerUsername: u.InfraServerUsername,
		CredentialId:        u.CredentialID,
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

// fromStorageToListAutomateInfraServerOrg: Create a response.AutomateInfraServerOrg from a storage.Org
func fromStorageToListAutomateInfraServerOrg(o storage.Org) *response.AutomateInfraServerOrg {
	return &response.AutomateInfraServerOrg{
		Id:           o.ID,
		Name:         o.Name,
		AdminUser:    o.AdminUser,
		CredentialId: o.CredentialID,
		ServerId:     o.ServerID,
		Projects:     o.Projects,
	}
}

//fromStorageToListAutomateInfraServerOrgs: Create a response.AutomateInfraServerOrg from an array of storage.Org
func fromStorageToListAutomateInfraServerOrgs(ol []storage.Org) []*response.AutomateInfraServerOrg {
	tl := make([]*response.AutomateInfraServerOrg, len(ol))

	for i, org := range ol {
		tl[i] = fromStorageToListAutomateInfraServerOrg(org)
	}

	return tl
}
