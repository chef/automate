package testDB

import (
	"context"
	"errors"
	"github.com/chef/automate/components/infra-proxy-service/storage"
)

type TestDB struct {
	Orgs      map[string]storage.Org
	Servers   map[string]storage.Server
	Users     map[string]storage.User
	NeedError bool
	Type      string
}

func (t *TestDB) GetServer(_ context.Context, id string) (storage.Server, error) {
	if t.NeedError {
		return storage.Server{}, errors.New("failed to fetch Server")
	}
	if _, ok := t.Servers[id]; !ok {
		return storage.Server{}, errors.New("No such server found")
	}
	return t.Servers[id], nil
}

func (t *TestDB) GetServers(ctx context.Context) ([]storage.Server, error) {
	x := make([]storage.Server, 10)
	if t.NeedError {
		return x, errors.New("failed to fetch Server")
	}
	for _, v := range t.Servers {
		x = append(x, v)
	}
	return x, nil
}

func (t *TestDB) StoreServer(ctx context.Context, id string, name string, fqdn string, ipAddress string, credentialId string) (storage.Server, error) {
	panic("implement me")
}

func (t *TestDB) DeleteServer(ctx context.Context, id string) (storage.Server, error) {
	panic("implement me")
}

func (t *TestDB) EditServer(ctx context.Context, id string, name string, fqdn string, ipAddress string) (storage.Server, error) {
	panic("implement me")
}

func (t *TestDB) EditServerWebuiKey(ctx context.Context, id, credentialId string) (storage.Server, error) {
	panic("implement me")
}

func (t *TestDB) GetOrg(ctx context.Context, orgID string, serverID string) (storage.Org, error) {
	panic("implement me")
}

func (t *TestDB) GetOrgs(ctx context.Context, serverID string) ([]storage.Org, error) {
	var x []storage.Org
	if t.NeedError {
		return x, errors.New("failed to fetch Orgs")
	}
	if t.Type == "Insert" {
		return x, nil
	}
	if t.Type == "Update" {
		org2 := storage.Org{
			ID:   "org2",
			Name: "Org",
		}
		x = append(x, org2)
		return x, nil
	}
	if t.Type == "Delete" {
		org2 := storage.Org{
			ID:   "org2",
			Name: "Org",
		}
		x = append(x, org2)
		return x, nil
	}
	if t.Type == "Skip" {
		org3 := storage.Org{
			ID:   "org3",
			Name: "Org 3",
		}
		x = append(x, org3)
		return x, nil
	}
	return x, nil
}

func (t *TestDB) StoreOrg(ctx context.Context, id string, name string, adminUser string, adminKey string, serverID string, projects []string) (storage.Org, error) {
	if t.NeedError {
		return storage.Org{}, errors.New("failed to store org")
	}
	return storage.Org{ID: id, Name: name, AdminUser: adminUser, CredentialID: adminKey, ServerID: serverID, Projects: projects}, nil
}

func (t *TestDB) DeleteOrg(ctx context.Context, orgID string, serverID string) (storage.Org, error) {
	if t.NeedError {
		return storage.Org{}, errors.New("failed to delete org")
	}
	return storage.Org{ID: orgID}, nil
}

func (t *TestDB) EditOrg(ctx context.Context, id string, name string, adminUser string, serverID string, projects []string) (storage.Org, error) {
	if t.NeedError {
		return storage.Org{}, errors.New("failed to edit org")
	}
	return storage.Org{ID: id, Name: name, AdminUser: adminUser, ServerID: serverID, Projects: projects}, nil
}

func (t *TestDB) TouchOrg(ctx context.Context, id string, serverID string) (storage.Org, error) {
	panic("implement me")
}

func (t *TestDB) InsertUser(ctx context.Context, id, serverID, infraServerUsername, credentialID, Connector, automateUserID string, IsServerAdmin bool) (storage.User, error) {
	panic("implement me")
}

func (t *TestDB) GetUser(ctx context.Context, id string) (storage.User, error) {
	panic("implement me")
}

func (t *TestDB) EditUser(ctx context.Context, id, serverID, infraServerUsername, credentialID, Connector, automateUserID string, IsServerAdmin bool) (storage.User, error) {
	panic("implement me")
}

func (t *TestDB) DeleteUser(ctx context.Context, id string) (storage.User, error) {
	panic("implement me")
}

func (t *TestDB) GetAutomateInfraServerUsers(ctx context.Context, serverId string) ([]storage.User, error) {
	panic("implement me")
}

func (t *TestDB) GetAutomateOrgUsers(ctx context.Context, orgId string) ([]storage.OrgUser, error) {
	var x []storage.OrgUser
	if t.NeedError {
		return x, errors.New("failed to fetch Orgs")
	}
	if t.Type == "Insert" {
		return x, nil
	}
	if t.Type == "Skip" || t.Type == "Delete" {
		x = append(x, storage.OrgUser{
			OrgId: orgId, InfraServerUsername: "user1",
		})
		x = append(x, storage.OrgUser{
			OrgId: orgId, InfraServerUsername: "user2",
		})
	}
	return x, nil
}
