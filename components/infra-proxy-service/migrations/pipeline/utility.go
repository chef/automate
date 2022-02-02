package pipeline

import (
	"context"

	"github.com/chef/automate/components/infra-proxy-service/storage"
)

// StoreOrgs reads the Result struct and populate the orgs table
func StoreOrgs(ctx context.Context, st storage.Storage, mst storage.MigrationStorage, res Result) (Result, error) {
	var err error
	var msg string
	var totalSucceeded, totalSkipped, totalFailed int64
	_, err = mst.StartOrgMigration(ctx, res.Meta.MigrationID, res.Meta.ServerID)
	if err != nil {
		return res, err
	}
	for _, org := range res.ParsedResult.Orgs {
		err, _ = StoreOrg(ctx, st, org, res.Meta.ServerID)
		if err != nil {
			msg = err.Error()
			totalFailed++
			break
		}
		if org.ActionOps == Skip {
			totalSkipped++
			continue
		}
		totalSucceeded++
	}
	if len(res.ParsedResult.Orgs) == int(totalFailed) {
		_, err = mst.FailedOrgMigration(ctx, res.Meta.MigrationID, res.Meta.ServerID, msg, totalSucceeded, totalSkipped, totalFailed)
		if err != nil {
			return res, err
		}
		return res, err
	}
	_, err = mst.CompleteOrgMigration(ctx, res.Meta.MigrationID, res.Meta.ServerID, totalSucceeded, totalSkipped, totalFailed)
	if err != nil {
		return res, err
	}
	return res, err
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
