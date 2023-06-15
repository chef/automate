package externalpostgresqlservice

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/db"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
)

type ExternalPostgresqlService interface {
	GetPgConnection(*models.ExternalPgRequest) (*models.ExternalPgResponse, error)
}

type ExternalPostgresqlServiceImpl struct {
	logger    logger.Logger
	DBUtils   db.DB
	fileUtils fileutils.FileUtils
	Req       *models.ExternalPgRequest
}

func NewExternalPostgresqlService(db db.DB, fileutils fileutils.FileUtils, logger logger.Logger) *ExternalPostgresqlServiceImpl {
	return &ExternalPostgresqlServiceImpl{
		logger:    logger,
		DBUtils:   db,
		fileUtils: fileutils,
	}
}

func (pg *ExternalPostgresqlServiceImpl) GetPgConnection(req *models.ExternalPgRequest) (*models.ExternalPgResponse, error) {
	pg.Req = req

	//creating a temp file to copy the content of a rootcert into a file
	rootcert, err := pg.fileUtils.CreateTempFile(req.PostgresqlRootCert, "root-cert")
	if err != nil {
		return nil, err
	}

	var resp *models.ExternalPgResponse
	err = pg.CheckExternalPgConnection(rootcert)
	if err != nil {
		pg.logger.Error(err)
		resp = &models.ExternalPgResponse{
			Passed: false,
			Checks: []models.ExternalPgConnectionDetails{failResponse(constants.EXTERNAL_PG_FAIL_CONNECTION_TITLE, constants.EXTERNAL_PG_CONNECTION_ERROR_MSG, constants.EXTERNAL_PG_CONNECTION_RESOLUTION_MSG)},
		}
		return resp, nil
	}

	resp = &models.ExternalPgResponse{
		Passed: true,
		Checks: []models.ExternalPgConnectionDetails{successResponse(constants.EXTERNAL_PG_SUCCESS_CONNECTION_TITLE, constants.EXTERNAL_PG_CONNECTION_SUCCESS_MSG)},
	}

	// delete the file when its done
	defer pg.fileUtils.DeleteFile(rootcert)

	return resp, nil
}
func (p *ExternalPostgresqlServiceImpl) CheckExternalPgConnection(rootcert string) error {

	connStr := fmt.Sprintf("host=%s port=%s user=%s "+
		"password=%s dbname=postgres sslmode=verify-ca sslrootcert=%s",
		p.Req.PostgresqlInstanceUrl, p.Req.PostgresqlInstancePort, "%s", "%s", rootcert)

	superUserConnStr := fmt.Sprintf(connStr, p.Req.PostgresqlSuperUserUserName, p.Req.PostgresqlSuperUserPassword)
	DbUserConnStr := fmt.Sprintf(connStr, p.Req.PostgresqlDbUserUserName, p.Req.PostgresqlDbUserPassword)

	QueryStr := []string{superUserConnStr, DbUserConnStr}

	for i := 0; i < len(QueryStr); i++ {
		err := p.DBUtils.InitPostgresDB(QueryStr[i])
		if err != nil {
			p.logger.Error("External Postgresql Connection failed: ", err.Error())
			return err
		}
		p.logger.Info("External Postgresql aws connection success", i)
	}
	return nil
}

func successResponse(Title string, SuccessMsg string) models.ExternalPgConnectionDetails {
	Resp := models.ExternalPgConnectionDetails{
		Title:         constants.EXTERNAL_PG_SUCCESS_CONNECTION_TITLE,
		Passed:        true,
		SuccessMsg:    constants.EXTERNAL_PG_CONNECTION_SUCCESS_MSG,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
	return Resp
}
func failResponse(Title string, ErrorMsg string, ResolutionMsg string) models.ExternalPgConnectionDetails {
	Resp := models.ExternalPgConnectionDetails{
		Title:         constants.EXTERNAL_PG_FAIL_CONNECTION_TITLE,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      constants.EXTERNAL_PG_CONNECTION_ERROR_MSG,
		ResolutionMsg: constants.EXTERNAL_PG_CONNECTION_RESOLUTION_MSG,
	}
	return Resp
}
