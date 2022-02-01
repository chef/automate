package test

import (
	"context"
	"github.com/chef/automate/components/infra-proxy-service/storage"
)

func (t *DB) GetOrgs(ctx context.Context, serverID string) ([]storage.Org, error) {
	var orgs []storage.Org
	org1 := storage.Org{
		ID:       "testo",
		Name:     "test-name",
		ServerID: serverID,
	}
	orgs = append(orgs, org1)
	return orgs, nil

}
