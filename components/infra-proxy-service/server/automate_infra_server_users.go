package server

import (
	"context"
	"fmt"

	secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/components/infra-proxy-service/validation"
	chef "github.com/go-chef/chef"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
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

// ResetInfraServerUserKey updates the public key on the Chef Server and returns the private key
func (s *Server) ResetInfraServerUserKey(ctx context.Context, req *request.ResetInfraServerUserKeyReq) (*response.ResetInfraServerUserKeyRes, error) {
	err := validation.New(validation.Options{
		Target:  "user",
		Request: *req,
		Rules: validation.Rules{
			"ServerId": []string{"required"},
			"Name":     []string{"required"},
		},
	}).Validate()

	if err != nil {
		log.Error("failed to validate: ", err)
		return nil, err
	}

	server, err := s.service.Storage.GetServer(ctx, req.ServerId)
	if err != nil {
		log.Error("cannot get the server: ", err)
		return nil, err
	}
	if server.CredentialID == "" {
		return nil, errors.New("webui key is not available with server")
	}
	// Get web ui key from secrets service
	secret, err := s.service.Secrets.Read(ctx, &secrets.Id{Id: server.CredentialID})
	if err != nil {
		log.Error("cannot read the secret: ", err)
		return nil, err
	}
	c, err := s.createChefServerClient(ctx, req.ServerId, GetAdminKeyFrom(secret), "pivotal", true)
	if err != nil {
		log.Error("cannot create a client: ", err)
		return nil, err
	}

	// Deletes the existing key
	_, err = c.client.Users.DeleteKey(req.UserName, "default")
	chefError, _ := chef.ChefError(err)
	if err != nil && chefError.StatusCode() != 404 {
		log.Error("cannot connect to the chef server: ", err)
		return nil, ParseAPIError(err)
	}

	// Add new key to existing client
	body, err := chef.JSONReader(AccessKeyReq{
		Name:           "default",
		ExpirationDate: "infinity",
		CreateKey:      true,
	})
	if err != nil {
		log.Error("cannot add key to the client: ", err)
		return nil, ParseAPIError(err)
	}

	var chefKey chef.ChefKey
	addReq, err := c.client.NewRequest("POST", fmt.Sprintf("users/%s/keys", req.UserName), body)

	if err != nil {
		log.Error("cannot create a request: ", err)
		return nil, ParseAPIError(err)
	}

	res, err := c.client.Do(addReq, &chefKey)
	if res != nil {
		log.Error("received nil response")
		defer res.Body.Close() //nolint:errcheck
	}

	if err != nil {
		log.Error("error occurred while sending a request: ", err)
		return nil, ParseAPIError(err)
	}

	return &response.ResetInfraServerUserKeyRes{
		UserId:     req.UserId,
		UserName:   req.UserName,
		ServerId:   req.ServerId,
		PrivateKey: chefKey.PrivateKey,
	}, nil
}
