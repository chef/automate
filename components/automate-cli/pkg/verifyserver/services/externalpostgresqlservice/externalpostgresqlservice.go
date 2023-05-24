package externalpostgresqlservice

import (
	"fmt"
	"io/ioutil"
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
	rootcert, err := createTempFile(req.PostgresqlRootCert)
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
		return resp,nil
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
		p.logger.Info("External Postgresql aws connection success")
	}
	return nil
}

func createTempFile(content string) (string, error) {

	tempFile, err := ioutil.TempFile("", "root-cert") // nosemgrep
	if err != nil {
		return "", errors.Wrap(err,"file creation failed ")
	}
	_, err = tempFile.WriteString((content))
	if err != nil {
		return "", errors.Wrap(err,"writing rootca to a file failed")
	}
	return tempFile.Name(), nil
}

func deleteTempFile(tempFile string) error {

	err := os.Remove(tempFile)
	if err != nil{
		return err
	}
	//check if the file still exists
	if _,err := os.Stat(tempFile); err == nil {
		return errors.Wrap(err,"Failed to delete file")
	}
	return nil
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
