package pipeline

import (
	"context"
	"fmt"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/api/interservice/local_user"
	"github.com/chef/automate/components/infra-proxy-service/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/storage"
	log "github.com/sirupsen/logrus"
)

// StoreOrgs reads the Result struct and populate the orgs table
func StoreOrgs(ctx context.Context, st storage.Storage, authzProjectClient authz.ProjectsServiceClient, res pipeline.Result) (pipeline.Result, error) {
	var err error

	log.Info("Starting the organization migration phase for migration id: ", res.Meta.MigrationID)
	for _, org := range res.ParsedResult.Orgs {
		if org.ActionOps == pipeline.Skip {
			res.ParsedResult.OrgsCount.Skipped++
			continue
		}
		err, _ = StoreOrg(ctx, st, org, res.Meta.ServerID, authzProjectClient)
		if err != nil {
			res.ParsedResult.OrgsCount.Failed++
			continue
		}
		res.ParsedResult.OrgsCount.Succeeded++
	}
	if len(res.ParsedResult.Orgs) == int(res.ParsedResult.OrgsCount.Failed) {
		log.Errorf("Failed to migrate orgs for migration id %s : %s", res.Meta.MigrationID, err.Error())
		return res, err
	}
	log.Info("Successfully completed the organization migration phase for migration id: ", res.Meta.MigrationID)
	return res, err
}

// StoreOrg stores a single Org into DB
func StoreOrg(ctx context.Context, st storage.Storage, org pipeline.Org, serverID string, authzProjectClient authz.ProjectsServiceClient) (error, pipeline.ActionOps) {
	var actionTaken pipeline.ActionOps
	var err error
	switch org.ActionOps {
	case pipeline.Insert:
		projects, err := createProjectFromOrgIdAndServerID(ctx, serverID, org.Name, authzProjectClient)
		if err != nil {
			log.Errorf("Unable to create project for serverid: %s", serverID)
			return err, actionTaken
		}
		_, err = st.StoreOrg(ctx, org.Name, org.FullName, "", "", serverID, projects)
		if err != nil {
			log.Errorf("Unable to insert org for server id: %s", serverID)
		}
		actionTaken = pipeline.Insert
	case pipeline.Delete:
		_, err = st.DeleteOrg(ctx, org.Name, serverID)
		actionTaken = pipeline.Delete
	case pipeline.Update:
		_, err = st.EditOrg(ctx, org.Name, org.FullName, "", serverID, nil)
		actionTaken = pipeline.Update
	default:
	}
	return err, actionTaken
}

//function to create a new iam project for each client
func createProjectFromOrgIdAndServerID(ctx context.Context, serverId string, orgId string, authzProjectClient authz.ProjectsServiceClient) ([]string, error) {

	newProject := &authz.CreateProjectReq{
		Name:         serverId + "_" + orgId,
		Id:           serverId + "_" + orgId,
		SkipPolicies: false,
	}

	projectID, err := authzProjectClient.CreateProject(ctx, newProject)
	if err != nil {
		return nil, err
	}

	return []string{projectID.Project.Name}, nil
}

// StoreOrgsUsersAssociation  populate the users association with orgs based on the actions
func StoreOrgsUsersAssociation(ctx context.Context, st storage.Storage, result pipeline.Result) (pipeline.Result, error) {
	log.Info("Starting with the storing org user association for migration id :", result.Meta.MigrationID)

	var err error
	var totalUsers int64
	selectedUsersMap := createMapForUsers(result.ParsedResult.Users)
	for _, orgUsers := range result.ParsedResult.OrgsUsers {
		for _, orgUserAssociation := range orgUsers.Users {
			if _, keyPresent := selectedUsersMap[orgUserAssociation.Username]; keyPresent {
				totalUsers++
				if orgUserAssociation.ActionOps == pipeline.Skip {
					result.ParsedResult.OrgsUsersAssociationsCount.Skipped++
					continue
				}
				err, _ = storeOrgUserAssociation(ctx, st, result.Meta.ServerID, orgUsers.OrgName.Name, orgUserAssociation)
				if err != nil {
					log.Errorf("Failed to migrate org user %s of org %s for migration id %s : %s", orgUserAssociation.Username, orgUsers.OrgName.Name, result.Meta.MigrationID, err.Error())
					result.ParsedResult.OrgsUsersAssociationsCount.Failed++
					continue
				}
				result.ParsedResult.OrgsUsersAssociationsCount.Succeeded++
			}
		}
	}
	if totalUsers == result.ParsedResult.OrgsUsersAssociationsCount.Failed {
		return result, err
	}
	log.Info("Completed with the storing org user association for migration id :", result.Meta.MigrationID)
	return result, nil
}

func createMapForUsers(users []pipeline.User) map[string]pipeline.User {
	usersMap := make(map[string]pipeline.User)
	for _, user := range users {
		usersMap[user.Username] = user
	}
	return usersMap
}

// storeOrgUserAssociation stores a single Org user into DB
func storeOrgUserAssociation(ctx context.Context, st storage.Storage, serverID string, orgID string, orgUserAssociation pipeline.UserAssociation) (error, pipeline.ActionOps) {
	var actionTaken pipeline.ActionOps
	var err error
	switch actionTaken = orgUserAssociation.ActionOps; orgUserAssociation.ActionOps {
	case pipeline.Insert:
		_, err = st.StoreOrgUserAssociation(ctx, serverID, orgID, orgUserAssociation.Username, orgUserAssociation.IsAdmin)
	case pipeline.Delete:
		_, err = st.DeleteOrgUserAssociation(ctx, serverID, orgID, orgUserAssociation.Username, orgUserAssociation.IsAdmin)
	case pipeline.Update:
		_, err = st.EditOrgUserAssociation(ctx, serverID, orgID, orgUserAssociation.Username, orgUserAssociation.IsAdmin)
	default:
	}
	return err, actionTaken
}

// StoreUsers reads the Result struct and populate the users table
func StoreUsers(ctx context.Context, st storage.Storage, localUserClient local_user.UsersMgmtServiceClient, res pipeline.Result) (pipeline.Result, error) {
	var err error

	for _, user := range res.ParsedResult.Users {
		if user.ActionOps == pipeline.Skip {
			res.ParsedResult.UsersCount.Skipped++
			continue
		}
		_, err = StoreUser(ctx, st, user, res.Meta.ServerID, localUserClient)
		if err != nil {
			res.ParsedResult.UsersCount.Failed++
			continue
		}
		res.ParsedResult.UsersCount.Succeeded++
	}

	if len(res.ParsedResult.Users) == int(res.ParsedResult.UsersCount.Failed) {
		log.Errorf("Failed to migrate user for migration id %s : %s", res.Meta.MigrationID, err)
		return res, err
	}

	log.Info("Successfully completed the user migration phase for migration id: ", res.Meta.MigrationID)
	return res, err
}

// StoreUser stores a single User into DB
func StoreUser(ctx context.Context, st storage.Storage, user pipeline.User, serverID string, localUserClient local_user.UsersMgmtServiceClient) (pipeline.ActionOps, error) {
	var actionTaken pipeline.ActionOps
	var err error

	storageUser := storage.User{
		ServerID:            serverID,
		InfraServerUsername: user.Username,
		Connector:           user.Connector,
		Email:               user.Email,
		DisplayName:         user.DisplayName,
		FirstName:           user.FirstName,
		LastName:            user.LastName,
		MiddleName:          user.MiddleName,
		AutomateUserID:      user.AutomateUsername,
	}
	switch actionTaken = user.ActionOps; user.ActionOps {
	case pipeline.Insert:
		if user.Connector == pipeline.Local {
			err = createLocalUser(ctx, localUserClient, user)
			if err != nil {
				return actionTaken, err
			}
		}
		_, err = st.InsertUser(ctx, storageUser)
	case pipeline.Delete:
		_, err = st.DeleteUser(ctx, storageUser)
	case pipeline.Update:
		_, err = st.EditUser(ctx, storageUser)
	default:
	}
	return actionTaken, err
}

//Populating User permissions for admin and non-admin members
func MigrateUsersPermissions(ctx context.Context, authzPolicyClient authz.PoliciesServiceClient, result pipeline.Result) (pipeline.Result, error) {
	log.Info("Starting with the migrating user permissions for migration id :", result.Meta.MigrationID)
	var err error
	var totalUsers int64
	selectedUsersMap := createMapForUsers(result.ParsedResult.Users)
	for _, orgUsers := range result.ParsedResult.OrgsUsers {
		for _, orgUserAssociation := range orgUsers.Users {
			if _, keyPresent := selectedUsersMap[orgUserAssociation.Username]; keyPresent {
				totalUsers++
				if orgUserAssociation.ActionOps == pipeline.Skip {
					result.ParsedResult.UserPermission.Skipped++
					continue
				}
				err, _ = MigrateUserPermission(ctx, orgUsers.OrgName, selectedUsersMap[orgUserAssociation.Username], orgUserAssociation, authzPolicyClient, result.Meta.ServerID)
				if err != nil {
					log.Errorf("Failed to migrate user permissions for org user %s of org %s for migration id %s : %s", orgUserAssociation.Username, orgUsers.OrgName.Name, result.Meta.MigrationID, err.Error())
					result.ParsedResult.UserPermission.Failed++
					continue
				}
				result.ParsedResult.UserPermission.Succeeded++
			}
		}
	}
	if totalUsers == result.ParsedResult.UserPermission.Failed {
		return result, err
	}
	log.Info("Completed with the migrating user permission for migration id :", result.Meta.MigrationID)
	return result, nil
}

func MigrateUserPermission(ctx context.Context, org pipeline.Org, user pipeline.User, orgUserAssociation pipeline.UserAssociation, authzPolicyClient authz.PoliciesServiceClient, serverId string) (error, pipeline.ActionOps) {
	var actionTaken pipeline.ActionOps
	var err error
	switch actionTaken = orgUserAssociation.ActionOps; orgUserAssociation.ActionOps {
	case pipeline.Insert:
		err = addUserToPolicy(ctx, org, user, orgUserAssociation.IsAdmin, authzPolicyClient, serverId)
	case pipeline.Delete:
		err = removePolicyFromUser(ctx, org, user, orgUserAssociation.IsAdmin, authzPolicyClient, serverId)
	case pipeline.Update:
		err = addUserToPolicy(ctx, org, user, orgUserAssociation.IsAdmin, authzPolicyClient, serverId)
		if err == nil {
			err = removePolicyFromUser(ctx, org, user, !orgUserAssociation.IsAdmin, authzPolicyClient, serverId)
		}
	default:
	}
	return err, actionTaken
}

func addUserToPolicy(ctx context.Context, org pipeline.Org, user pipeline.User, isAdmin bool, authzPolicyClient authz.PoliciesServiceClient, serverId string) error {
	log.Info("Adding policy for the user", user.AutomateUsername)
	policyId := getPolicyId(isAdmin, org, serverId)
	member := fmt.Sprintf("user:%s:%s", user.Connector, user.AutomateUsername)
	members := []string{member}
	createPolicyRequest := &authz.AddPolicyMembersReq{Id: policyId, Members: members}
	_, err := authzPolicyClient.AddPolicyMembers(ctx, createPolicyRequest)
	if err != nil {
		log.Errorf("Unable to add policy for the username %s", user.AutomateUsername)
		return err
	}
	return nil
}

func removePolicyFromUser(ctx context.Context, org pipeline.Org, user pipeline.User, isAdmin bool, authzPolicyClient authz.PoliciesServiceClient, serverId string) error {
	log.Info("Deleting policy for the user", user.AutomateUsername)
	policyId := getPolicyId(isAdmin, org, serverId)
	member := fmt.Sprintf("user:%s:%s", user.Connector, user.AutomateUsername)
	removePolicyRequest := &authz.RemovePolicyMembersReq{Id: policyId, Members: []string{member}}
	_, err := authzPolicyClient.RemovePolicyMembers(ctx, removePolicyRequest)
	if err != nil {
		log.Errorf("Unable to delete policy for the username %s", user.AutomateUsername)
		return err
	}
	return nil
}

func getPolicyId(isAdmin bool, org pipeline.Org, serverId string) string {
	var policyId string
	if isAdmin {
		policyId = fmt.Sprintf("%s_%s-%s", serverId, org.Name, "project-owners")
	} else {
		policyId = fmt.Sprintf("%s_%s-%s", serverId, org.Name, "project-editors")
	}
	return policyId
}
