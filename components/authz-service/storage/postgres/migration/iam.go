package migration

import (
	"context"
	"database/sql"
	"fmt"

	"github.com/pkg/errors"
)

const (
	enumPristine        = "init"
	enumInProgress      = "in-progress"
	enumSuccessful      = "successful"
	enumSuccessfulBeta1 = "successful-beta1"
	enumFailed          = "failed"
)

func needsV2Migration(ctx context.Context, db *sql.DB) (bool, error) {
	var status string
	row := db.QueryRowContext(ctx, `SELECT state FROM migration_status`)
	err := row.Scan(&status)
	if err != nil {
		return true, err // shouldn't happen, migration initializes state
	}
	switch status {
	case enumPristine:
		return true, nil
	case enumSuccessful:
		return false, nil
	case enumSuccessfulBeta1:
		return false, nil
	// TODO how should we properly handle these cases? is re-running migration enough?
	case enumInProgress:
		return true, nil
	case enumFailed:
		return true, nil
	}
	return true, fmt.Errorf("unexpected migration status: %q", status)
}

// MigrateToV2 sets the V2 store to its factory defaults and then migrates
// any existing V1 policies, unless the install is already on IAM v2.
func migrateToV2(ctx context.Context, db *sql.DB) error {
	ifNotOnV2, err := needsV2Migration(ctx, db)
	if err != nil {
		return errors.Wrap(err, "could not query IAM migration state")
	}

	// for _, role := range storage.DefaultRoles() {
	// 	if _, err := s.store.CreateRole(ctx, &role, true); err != nil {
	// 		return nil, status.Errorf(codes.Internal, "reset to default roles: %s", err.Error())
	// 	}
	// }

	// defaultPolicies, err := storage.DefaultPolicies()
	// if err != nil {
	// 	return nil, status.Errorf(codes.Internal, "retrieve default policies: %s", err.Error())
	// }

	// for _, pol := range defaultPolicies {
	// 	if _, err := s.store.CreatePolicy(ctx, &pol, true); err != nil {
	// 		return nil, status.Errorf(codes.Internal, "reset to default policies: %s", err.Error())
	// 	}
	// }

	// Should be able to drop this code. Happening in migration 74_iam_v2_forced_upgrade...
	// // Added for testing only; these are handled by data migrations.
	// for _, project := range storage.DefaultProjects() {
	// 	if _, err := s.store.CreateProject(ctx, &project, false); err != nil {
	// 		return nil, status.Errorf(codes.Internal, "reset to default project: %s", err.Error())
	// 	}
	// }

	// recordFailure := func() {
	// 	// This should be unlikely, and it doesn't affect our returned error, which,
	// 	// in any case, is the more interesting error -- so, we merely log it.
	// 	if err := s.store.Failure(ctx); err != nil {
	// 		s.log.Errorf("failed to record migration failure status: %s", err)
	// 	}
	// }

	// var reports []string
	// if !req.SkipV1Policies {
	// 	errs, err := s.migrateV1Policies(ctx)
	// 	if err != nil {
	// 		recordFailure()
	// 		return nil, status.Errorf(codes.Internal, "migrate v1 policies: %s", err.Error())
	// 	}
	// 	for _, e := range errs {
	// 		reports = append(reports, e.Error())
	// 	}
	// } else {
	// 	// Note 2019/05/22 (sr): policies without subjects are silently ignored -- this
	// 	// is to be in line with the migration case, that does the same. However, this
	// 	// could be worth revisiting?
	// 	pols, err := s.v1.ListPoliciesWithSubjects(ctx)
	// 	if err != nil {
	// 		recordFailure()
	// 		return nil, status.Errorf(codes.Internal, "list v1 policies: %s", err.Error())
	// 	}
	// 	reports = append(reports, fmt.Sprintf("%d v1 policies", len(pols)))

	// }

	// // we've made it!
	// var v api.Version
	// switch req.Flag {
	// case api.Flag_VERSION_2_1:
	// 	err = s.store.SuccessBeta1(ctx)
	// 	v = api.Version{Major: api.Version_V2, Minor: api.Version_V1}
	// default:
	// 	err = s.store.Success(ctx)
	// 	v = api.Version{Major: api.Version_V2, Minor: api.Version_V0}
	// }
	// if err != nil {
	// 	recordFailure()
	// 	return nil, status.Errorf(codes.Internal, "record migration status: %s", err.Error())
	// }

	// s.setVersionForInterceptorSwitch(v)
	// return &api.MigrateToV2Resp{Reports: reports}, nil

	return nil
}
