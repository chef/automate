package gcp

import (
	"context"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/oauth2/google"
	compute "google.golang.org/api/compute/v1"

	"github.com/chef/automate/api/external/secrets"
)

type GcpCreds struct {
	asString string
	asStruct *secrets.GcpCredential
}

func New(secret *secrets.Secret) (gcpCreds *GcpCreds, err error) {
	gcpCreds = &GcpCreds{
		asString: "",
	}
	for _, item := range secret.Data {
		if item.Key == "GOOGLE_CREDENTIALS_JSON" {
			gcpCreds.asString = item.Value
		}
	}

	if len(gcpCreds.asString) == 0 {
		return nil, errors.Wrap(err, "New unable to retrieve GOOGLE_CREDENTIALS_JSON")
	} else {
		logrus.Infof("googleApplicationCredentials credential len: %d", len(gcpCreds.asString))
	}

	gcpCreds.asStruct, err = secrets.UnmarshalGcpServiceAcc(gcpCreds.asString)
	if err != nil {
		return nil, errors.Wrap(err, "gcp manager New")
	}

	return gcpCreds, nil
}

func (creds *GcpCreds) TestConnectivity(ctx context.Context) error {
	googConfig, err := google.JWTConfigFromJSON([]byte(creds.asString), compute.ComputeScope)
	if err != nil {
		return errors.Wrap(err, "gcp unable to read creds")
	}
	client := googConfig.Client(ctx)

	service, err := compute.New(client)
	if err != nil {
		return errors.Wrap(err, "gcp TestConnectivity unable to create compute service")
	}
	projectsService := service.Projects
	projectsGet := projectsService.Get(creds.asStruct.ProjectID)
	project, err := projectsGet.Do()
	if err != nil {
		return errors.Wrap(err, "gcp TestConnectivity unable to compute.New")
	}
	logrus.Debugf("gcp TestConnectivity successful for project: %v", project.Id)

	return nil
}

func (creds *GcpCreds) GetProjectID(ctx context.Context) string {
	return creds.asStruct.ProjectID
}
