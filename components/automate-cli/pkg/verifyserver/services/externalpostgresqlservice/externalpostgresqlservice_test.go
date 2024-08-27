package externalpostgresqlservice_test

import (
	"errors"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/externalpostgresqlservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/db"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var (
	req = models.ExternalPgRequest{
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
	}, &fileutils.MockFileSystemUtils{
		CreateTempFileFunc: func(content string, filename string, dir string) (string, error) {
			return "", nil
		},
		DeleteTempFileFunc: func(tempFile string) error {
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
				Title:         constants.EXTERNAL_PG_SUCCESS_CONNECTION_TITLE,
				Passed:        true,
				SuccessMsg:    constants.EXTERNAL_PG_CONNECTION_SUCCESS_MSG,
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
	}, &fileutils.MockFileSystemUtils{
		CreateTempFileFunc: func(content string, filename string, dir string) (string, error) {
			return "", nil
		},
		DeleteTempFileFunc: func(tempFile string) error {
			return nil
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
				Title:         constants.EXTERNAL_PG_FAIL_CONNECTION_TITLE,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      constants.EXTERNAL_PG_CONNECTION_ERROR_MSG + "\n ",
				ResolutionMsg: constants.EXTERNAL_PG_CONNECTION_RESOLUTION_MSG,
			},
		},
	}, services)

}
