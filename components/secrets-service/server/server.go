package server

import (
	"context"

	"github.com/pkg/errors"
	logs "github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/secrets-service/dao"
	"github.com/chef/automate/components/secrets-service/utils"
	"github.com/chef/automate/lib/grpc/health"
)

// SecretsServer is the interface to this component.
type SecretsServer struct {
	secretsDb *dao.DB
	health    *health.Service
}

// New creates a new SecretsServer instance.
func New(secretsDb *dao.DB) *SecretsServer {
	return &SecretsServer{
		secretsDb: secretsDb,
		health:    health.NewService(),
	}
}

// Create a new secret
func (ss *SecretsServer) Create(ctx context.Context, in *secrets.Secret) (*secrets.Id, error) {
	if err := in.Validate(); err != nil {
		return nil, utils.FormatErrorMsg(errors.Wrap(err, "Create: unable to validate secret"), "")
	}
	sID, err := ss.secretsDb.AddSecret(in)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}
	return &secrets.Id{Id: sID}, nil
}

// Read a secret via ID
func (ss *SecretsServer) Read(ctx context.Context, in *secrets.Id) (*secrets.Secret, error) {
	logs.Infof("read secret with : %+v", in.Id)
	secret, err := ss.secretsDb.GetSecret(in.Id)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	return secret, nil
}

// Update one secret
func (ss *SecretsServer) Update(ctx context.Context, in *secrets.Secret) (*secrets.UpdateResponse, error) {
	logs.Infof("update secret with : %+v", in.Id)

	newSecret, err := ss.secretsDb.GetSecret(in.Id)
	if err != nil {
		return nil, utils.FormatErrorMsg(utils.ProcessSQLNotFound(err, in.Id, "updateSecretValidation error getting secret from db"), in.Id)
	}

	newSecret.Merge(in)

	// do some error checking
	err = newSecret.Validate()
	if err != nil {
		return nil, utils.FormatErrorMsg(utils.ProcessInvalid(err, "updateSecretValidation: unable to validate secret"), in.Id)
	}

	err = ss.secretsDb.UpdateSecret(newSecret)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	return &secrets.UpdateResponse{}, nil
}

// Delete a secret
func (ss *SecretsServer) Delete(ctx context.Context, in *secrets.Id) (*secrets.DeleteResponse, error) {
	logs.Infof("Deleting Secret id: %+v", in.Id)
	_, err := ss.secretsDb.DeleteSecret(in.Id)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	return &secrets.DeleteResponse{}, nil
}

// List secrets based on a query
func (ss *SecretsServer) List(ctx context.Context, in *secrets.Query) (*secrets.Secrets, error) {
	logs.Debugf("Getting Secrets with query: %+v", in)
	dbsecrets, totalCount, err := ss.secretsDb.GetSecrets(in.Sort, in.Order, in.Page, in.PerPage, in.Filters)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}
	return &secrets.Secrets{Secrets: dbsecrets, Total: int32(totalCount)}, nil
}

// Health returns the servers embedded health check service
func (ss *SecretsServer) Health() *health.Service {
	return ss.health
}
