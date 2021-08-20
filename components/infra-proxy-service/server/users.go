package server

import (
	"context"
	"fmt"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/api/interservice/infra_proxy/response"
	"github.com/go-chef/chef"
)

// GetUsersList Get a list of all users
func (s *Server) GetUsersList(ctx context.Context, req *request.Users) (*response.Users, error) {
	c, err := s.createClient(ctx, req.OrgId, req.ServerId)
	if err != nil {
		return nil, err
	}
	var usersList interface{}
	usersList, err = c.client.Users.VerboseList()
	fmt.Println("####################userlist##############", usersList)
	userswantList, err := c.client.Users.List()
	fmt.Println("####################userwantlist##############", userswantList)
	if err != nil {
		return nil, ParseAPIError(err)
	}
	return &response.Users{
		Users: fromAPIToListUsers(usersList),
	}, nil

}

func fromAPIToListUsers(list interface{}) []*response.UsersListItem {
	var users []*response.UsersListItem
	listun := list.(map[string]chef.UserVerboseResult)
	for _, user := range listun {
		//for p, rev := range policyList.Policies {
		fmt.Println("user---", user)
		item := &response.UsersListItem{
			FirstName: "user.FirstName",
			LastName:  "",
			Email:     "user.Email",
		}
		users = append(users, item)
		//}
	}

	return users
}
