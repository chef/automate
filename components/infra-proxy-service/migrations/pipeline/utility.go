package pipeline

import (
	"context"
)
import "github.com/chef/automate/components/infra-proxy-service/storage"

// StoreOrgs reads the Result struct and populate the orgs table
func StoreOrgs(ctx context.Context, st storage.Storage, res Result) error {
	var err error
	for _, org := range res.ParsedResult.Orgs {
		err, _ = StoreOrg(ctx, st, org, res.Meta.ServerID)
		if err != nil {
			break
		}
	}
	return err
}

// StoreOrg stores a single Org into DB
func StoreOrg(ctx context.Context, st storage.Storage, org Org, serverID string) (error, ActionOps) {
	var actionTaken ActionOps
	var err error
	switch org.ActionOps {
	case Insert:
		_, err = st.StoreOrg(ctx, org.Name, org.FullName, "", "", serverID, nil)
		actionTaken = Insert
	case Delete:
		_, err = st.DeleteOrg(ctx, org.Name, serverID)
		actionTaken = Delete
	case Update:
		_, err = st.EditOrg(ctx, org.Name, org.FullName, "", serverID, nil)
		actionTaken = Update
	default:
	}
	return err, actionTaken
}
