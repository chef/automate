// +build integration

package integration_test

import (
	"context"
	"database/sql"
	"os"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/platform/pg"
	"github.com/chef/automate/lib/workflow"
	"github.com/chef/automate/lib/workflow/postgres"
	"github.com/stretchr/testify/suite"
)

const defaultDatabaseName = "workflow_test"

func defaultConnURIForDatabase(dbname string) string {
	if os.Getenv("PG_URI") != "" {
		return os.Getenv("PG_URI")
	}
	connInfo := pg.A2ConnInfo{
		Host:  "localhost",
		Port:  5432,
		User:  "automate",
		Certs: pg.A2SuperuserCerts,
	}
	return connInfo.ConnURI(dbname)
}

func runResetDB() error {
	dbName := defaultDatabaseName

	db, err := sql.Open("postgres", defaultConnURIForDatabase("template1"))
	if err != nil {
		return errors.Wrap(err, "could not initialize db connection")
	}
	defer db.Close()
	_, err = db.Exec(pg.DropDatabaseQuery(dbName))
	if err != nil {
		return errors.Wrap(err, "could not drop database")
	}
	_, err = db.Exec(pg.CreateDatabaseQuery(dbName))
	if err != nil {
		return errors.Wrap(err, "could not create database")
	}
	return nil
}
func TestPostgresWorkflowManager(t *testing.T) {
	ctx := context.Background()
	require.NoError(t, runResetDB())
	pgBackend := postgres.NewPostgresBackend(defaultConnURIForDatabase(defaultDatabaseName))
	defer pgBackend.Close()
	s := &WorkflowTestSuite{
		newManager: func(opts ...workflowManagerOptFunc) *workflow.WorkflowManager {
			o := workflowManagerOpt{}
			for _, f := range opts {
				f(&o)
			}
			m, err := workflow.NewManager(pgBackend)
			require.NoError(t, err)
			for _, w := range o.WorkflowExecutors {
				err := m.RegisterWorkflowExecutor(w.Name, w.Executor)
				require.NoError(t, err)
			}
			for _, te := range o.TaskExecutors {
				err := m.RegisterTaskExecutor(te.Name, te.Executor, workflow.TaskExecutorOpts{})
				require.NoError(t, err)
			}
			if !o.NoStart {
				m.Start(ctx)
			}
			return m
		},
	}
	suite.Run(t, s)
	require.NoError(t, pgBackend.Close())
}
