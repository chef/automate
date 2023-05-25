package externalpostgresqlservice

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/db"
	"github.com/chef/automate/lib/logger"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
)

type ISExternalPostgresqlService interface {
	GetPgConnection(*models.ExternalPgRequest) (*models.ExternalPgResponse, error)
}

type ExternalPostgresqlService struct {
	logger  logger.Logger
	DBUtils db.DB
	Req     *models.ExternalPgRequest
}

func NewExternalPostgresqlService(db db.DB, logger logger.Logger) *ExternalPostgresqlService {
	return &ExternalPostgresqlService{
		logger:  logger,
		DBUtils: db,
	}
}

func (pg *ExternalPostgresqlService) GetPgConnection(req *models.ExternalPgRequest) (*models.ExternalPgResponse, error) {
	pg.Req = req

	//creating a temp file to the copy content of a rootcert into a file
	rootcert, err := pg.createTempFile(req.PostgresqlRootCert)
	if err != nil {
		return nil, err
	}

	var resp *models.ExternalPgResponse
	err = pg.CheckExternalPgConnection(rootcert)
	if err != nil {
		pg.logger.Error(err)
		resp = &models.ExternalPgResponse{
			Passed: false,
			Checks: []models.ExternalPgConnectionDetails{failResponse(constants.EXTERNALPGFAILCONNECTIONTITLE, constants.EXTERNALPGCONNECTIONERRORMSG, constants.EXTERNALPGCONNECTIONRESOLUTIONMSG)},
		}
		return resp, nil
	}

	resp = &models.ExternalPgResponse{
		Passed: true,
		Checks: []models.ExternalPgConnectionDetails{successResponse(constants.EXTERNALPGSUCCESSCONNECTIONTITLE, constants.EXTERNALPGCONNECTIONSUCCESSMSG)},
	}

	// delete the file when its done
	defer deleteTempFile(rootcert)

	return resp, nil
}
func (p *ExternalPostgresqlService) CheckExternalPgConnection(rootcert string) error {

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
		p.logger.Info("External Postgresql aws connection success",i)
	}
	return nil
}

func (pg *ExternalPostgresqlService) createTempFile(content string) (string, error) {

	tempFile, err := os.CreateTemp("", "root-cert")
	if err != nil {
		return "", errors.Wrap(err, "file creation failed ")
	}
	_, err = tempFile.WriteString((content))
	if err != nil {
		return "", errors.Wrap(err, "writing rootca to a file failed")
	}
	pg.logger.Debug("File created : "+tempFile.Name())
	return tempFile.Name(), nil
}

func deleteTempFile(tempFile string) error {

	return os.Remove(tempFile)
}

func successResponse(Title string, SuccessMsg string) models.ExternalPgConnectionDetails {
	Resp := models.ExternalPgConnectionDetails{
		Title:         constants.EXTERNALPGSUCCESSCONNECTIONTITLE,
		Passed:        true,
		SuccessMsg:    constants.EXTERNALPGCONNECTIONSUCCESSMSG,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
	return Resp
}
func failResponse(Title string, ErrorMsg string, ResolutionMsg string) models.ExternalPgConnectionDetails {
	Resp := models.ExternalPgConnectionDetails{
		Title:         constants.EXTERNALPGFAILCONNECTIONTITLE,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      constants.EXTERNALPGCONNECTIONERRORMSG,
		ResolutionMsg: constants.EXTERNALPGCONNECTIONRESOLUTIONMSG,
	}
	return Resp
}
