package v1

import (
	"errors"
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/gofiber/fiber/v2"
)

func (h *Handler) GetGCPCloudStorageConfig(c *fiber.Ctx) error {
	var bucketAccess *models.Checks
	var checks []models.Checks
	gcpConfigRequest := new(models.GCPCloudStorageConfigRequest)
	if err := c.BodyParser(&gcpConfigRequest); err != nil {
		errString := fmt.Sprintf("GCP config request body parsing failed: %v", err.Error())
		h.Logger.Error(fmt.Errorf(errString))
		return fiber.NewError(fiber.StatusBadRequest, err.Error())
	}

	if res, err := CredentialsCheck(gcpConfigRequest); err != nil {
		return c.JSON(response.BuildSuccessResponse(&models.GCPCloudStorageResponse{
			Passed: false,
			Checks: CredCheckErr(res, err),
		}))
	}

	gcpConnection := h.GCPConfigService.GetGCPConnection(c.Context(), gcpConfigRequest)
	if gcpConnection.Passed {
		bucketAccess = h.GCPConfigService.GetBucketAccess(c.Context(), gcpConfigRequest)
		checks = append(checks, []models.Checks{*gcpConnection, *bucketAccess}...)
	} else {
		checks = append(checks, *gcpConnection)
	}

	return c.JSON(response.BuildSuccessResponse(&models.GCPCloudStorageResponse{
		Passed: gcpConnection.Passed && bucketAccess.Passed,
		Checks: checks,
	}))
}

func CredentialsCheck(cred *models.GCPCloudStorageConfigRequest) (string, error) {
	if cred.GcpServiceAccount.Type == "" {
		return constants.GCS_TYPE_MISSING_RESOLUTION, errors.New(constants.GCS_TYPE_MISSING)
	}
	if cred.GcpServiceAccount.ProjectID == "" {
		return constants.GCS_PROJECT_ID_RESOLUTION, errors.New(constants.GCS_PROJECT_ID)
	}
	if cred.GcpServiceAccount.PrivateKeyID == "" {
		return constants.GCS_PRIVATE_KEY_ID_RESOLUTION, errors.New(constants.GCS_PRIVATE_KEY_ID)
	}
	if cred.GcpServiceAccount.PrivateKey == "" {
		return constants.GCS_PRIVATE_KEY_RESOLUTION, errors.New(constants.GCS_PRIVATE_KEY)
	}
	if cred.GcpServiceAccount.ClientEmail == "" {
		return constants.GCS_CLIENT_EMAIL_RESOLUTION, errors.New(constants.GCS_CLIENT_EMAIL)
	}
	if cred.GcpServiceAccount.ClientID == "" {
		return constants.GCS_CLIENT_ID_RESOLUTION, errors.New(constants.GCS_CLIENT_ID)
	}
	if cred.GcpServiceAccount.AuthURI == "" {
		return constants.GCS_AUTH_URI_RESOLUTION, errors.New(constants.GCS_AUTH_URI)
	}
	if cred.GcpServiceAccount.TokenURI == "" {
		return constants.GCS_TOKEN_URI_RESOLUTION, errors.New(constants.GCS_TOKEN_URI)
	}
	if cred.GcpServiceAccount.AuthProviderX509CertURL == "" {
		return constants.GCS_AUTH_PROVIDER_x509_CERT_URL_RESOLUTION, errors.New(constants.GCS_AUTH_PROVIDER_x509_CERT_URL)
	}
	if cred.GcpServiceAccount.ClientX509CertURL == "" {
		return constants.GCS_CLIENT_x509_CERT_URL_RESOLUTION, errors.New(constants.GCS_CLIENT_x509_CERT_URL)
	}
	if cred.GcpServiceAccount.UniverseDomain == "" {
		return constants.GCS_UNIVERSAL_DOMAIN_RESOLUTION, errors.New(constants.GCS_UNIVERSAL_DOMAIN)
	}
	return "", nil
}

func CredCheckErr(resolution string, err error) []models.Checks {
	return []models.Checks{
		{
			Title:         constants.GCP_CONNECTION_TITLE,
			Passed:        false,
			ErrorMsg:      err.Error(),
			ResolutionMsg: resolution,
		},
	}
}
