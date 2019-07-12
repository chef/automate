// +build integration

package integration_test

import (
	"context"
	"database/sql"
	"os"
	"testing"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"
	"github.com/stretchr/testify/suite"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/platform/pg"
)

const (
	defaultTestDatabaseName  = "cereal_test"
	defaultAdminDatabaseName = "template1"
)

var defaultA2ConnInfo = pg.A2ConnInfo{
	Host:  "localhost",
	Port:  5432,
	User:  "automate",
	Certs: pg.A2SuperuserCerts,
}

func adminDBURL() string {
	if os.Getenv("PG_ADMIN_URL") != "" {
		return os.Getenv("PG_ADMIN_URL")
	}
	return defaultA2ConnInfo.ConnURI(defaultAdminDatabaseName)
}

func testDBURL() string {
	if os.Getenv("PG_URL") != "" {
		return os.Getenv("PG_URL")
	}
	return defaultA2ConnInfo.ConnURI(defaultTestDatabaseName)
}

func runResetDB() error {
	db, err := sql.Open("postgres", adminDBURL())
	if err != nil {
		return errors.Wrap(err, "could not initialize db connection")
	}
	defer db.Close()
	_, err = db.Exec(pg.DropDatabaseQuery(defaultTestDatabaseName))
	if err != nil {
		return errors.Wrap(err, "could not drop database")
	}
	_, err = db.Exec(pg.CreateDatabaseQuery(defaultTestDatabaseName))
	if err != nil {
		return errors.Wrap(err, "could not create database")
	}
	return nil
}

func TestCerealPostgres(t *testing.T) {
	ctx := context.Background()
	require.NoError(t, runResetDB())
	pgBackend := postgres.NewPostgresBackend(testDBURL(), postgres.WithTaskPingInterval(3*time.Second))
	defer pgBackend.Close()
	s := &CerealTestSuite{
		newManager: func(opts ...managerOptFunc) *cereal.Manager {
			o := managerOpt{}
			for _, f := range opts {
				f(&o)
			}
			m, err := cereal.NewManager(pgBackend)
			require.NoError(t, err)
			for _, w := range o.WorkflowExecutors {
				err := m.RegisterWorkflowExecutor(w.Name, w.Executor)
				require.NoError(t, err)
			}
			for _, te := range o.TaskExecutors {
				err := m.RegisterTaskExecutor(te.Name, te.Executor, cereal.TaskExecutorOpts{})
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

func init() {
	logrus.SetLevel(logrus.DebugLevel)
}
