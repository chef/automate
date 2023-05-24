package externalpostgresqlservice_test

import (
	"errors"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalpostgresqlservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/db"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	ExternalPgSuccessConnectionTitle  = "Machine is able to connect with External Managed Postgres"
	ExternalPgFailConnectionTitle     = "External Postgresql Connection failed"
	ExternalPgConnectionErrorMsg      = "Machine is unable to connect with External Managed Postgresql"
	ExternalPgConnectionResolutionMsg = "Ensure that the Postgres configuration provided is correct. Review security group or firewall settings as well on the infrastructure"
	ExternalPgConnectionSuccessMsg    = "Connection successfully tested"
	req                               = models.ExternalPgRequest{
		PostgresqlInstanceUrl:       "A.B.C.D",
		PostgresqlInstancePort:      "7432",
		PostgresqlSuperUserUserName: "postgres",
		PostgresqlSuperUserPassword: "Progress123",
		PostgresqlDbUserUserName:    "postgres",
		PostgresqlDbUserPassword:    "Progress123",
		PostgresqlRootCert:          "----- BEGIN CERTIFICATE ------",
	}
)

func TestExternalPostgresqlService(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	cs := externalpostgresqlservice.NewExternalPostgresqlService(&db.MockDB{
		InitPostgresDBFunc: func(con string) error {
			return nil
		},
	}, log)
	services, _ := cs.GetPgConnection(&models.ExternalPgRequest{
		PostgresqlInstanceUrl:       "A.B.C.D",
		PostgresqlInstancePort:      "7432",
		PostgresqlSuperUserUserName: "postgres",
		PostgresqlSuperUserPassword: "Progress123",
		PostgresqlDbUserUserName:    "postgres",
		PostgresqlDbUserPassword:    "Progress123",
		PostgresqlRootCert:          "----- BEGIN CERTIFICATE ------",
	})
	assert.Equal(t, &models.ExternalPgResponse{
		Passed: true,
		Checks: []models.ExternalPgConnectionDetails{
			{
				Title:         ExternalPgSuccessConnectionTitle,
				Passed:        true,
				SuccessMsg:    ExternalPgConnectionSuccessMsg,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
	}, services)
}

func TestExternalPostgresqlServiceFailure(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	csf := externalpostgresqlservice.NewExternalPostgresqlService(&db.MockDB{
		InitPostgresDBFunc: func(con string) error {
			return errors.New("")
		},
	}, log)
	services, _ := csf.GetPgConnection(&models.ExternalPgRequest{
		PostgresqlInstanceUrl:       "A.B.C.D",
		PostgresqlInstancePort:      "7432",
		PostgresqlSuperUserUserName: "postgres",
		PostgresqlSuperUserPassword: "Progress123",
		PostgresqlDbUserUserName:    "postgres",
		PostgresqlRootCert:          "-----",
	})
	assert.Equal(t, &models.ExternalPgResponse{
		Passed: false,
		Checks: []models.ExternalPgConnectionDetails{
			{
				Title:         ExternalPgFailConnectionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      ExternalPgConnectionErrorMsg,
				ResolutionMsg: ExternalPgConnectionResolutionMsg,
			},
		},
	}, services)

}
