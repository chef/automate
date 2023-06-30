package certificatevalidation

import (
	"crypto/rsa"
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type IValidateCertificateService interface {
	CertificateValidation(models.CertificateCheckRequest) models.CertificateCheckResponse
}

type ValidateCertificateService struct {
	log logger.Logger
}

func NewValidateCertificateService(log logger.Logger) *ValidateCertificateService {
	return &ValidateCertificateService{
		log: log,
	}
}

func createCheck(title string, passed bool, successMsg, errorMsg, resolutionMsg string) models.Checks {
	return models.Checks{
		Title:         title,
		Passed:        passed,
		SuccessMsg:    successMsg,
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
}

func createErrorMessage(expiredCerts string) string {
	errorMessage := ""
	if expiredCerts != "" {
		expiredCerts = strings.TrimSuffix(expiredCerts, ", ")
		errorMessage = fmt.Sprintf(constants.CERTIFICATE_EXPIRY_ERROR_MESSAGE, expiredCerts)
	}

	return errorMessage
}

func decodeAndParseCertificate(certificate, key string) (*x509.Certificate, error) {
	block, _ := pem.Decode([]byte(certificate))
	if block == nil || block.Type != constants.CERTIFICATE_BLOCK_TYPE {
		return nil, fmt.Errorf("failed to decode %s certificate", key)
	}

	parsedCertificate, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		return nil, fmt.Errorf("failed to parse %s certificate: %s", key, err.Error())
	}

	return parsedCertificate, nil
}

func (vc *ValidateCertificateService) validateCertificateExpiry(certificates map[string]string, keys []string) models.Checks {
	vc.log.Debug("Validating Certificates Expiry...")
	expiredCerts := ""

	for _, key := range keys {
		cert := certificates[key]
		certificate, err := decodeAndParseCertificate(cert, key)
		if err != nil {
			vc.log.Error(err)
			expiredCerts += key + ", "
			continue
		}

		// this is for checking that certificate is expired or not.
		currentTime := time.Now()
		if currentTime.After(certificate.NotAfter) {
			vc.log.Debugf("%s certificate is expired", key)
			expiredCerts += key + ", "
			continue
		}

		// Is the certificate expiring in within 30 days
		threshold := currentTime.AddDate(0, 0, 30)
		if certificate.NotAfter.Before(threshold) {
			vc.log.Debugf("%s certificate is will expire soon", key)
			fmt.Printf("The certificate will expire with 30 days\n")
			continue
		}

	}

	if expiredCerts == "" {
		vc.log.Debug("Certificates are not expired")
		return createCheck(constants.CERTIFICATE_EXPIRY_TITLE, true, constants.CERTIFICATE_EXPIRY_SUCCESS_MESSAGE, "", "")
	}

	errorMessage := createErrorMessage(expiredCerts)
	return createCheck(constants.CERTIFICATE_EXPIRY_TITLE, false, "", errorMessage, constants.CERTIFICATE_EXPIRY_RESOLUTION_MESSAGE)
}

func (vc *ValidateCertificateService) validateCertificateFormat(certificates map[string]string, keys []string) models.Checks {
	vc.log.Debug("Validating Certificates Format...")
	invalidFormatCerts := ""

	for _, key := range keys {
		cert := certificates[key]
		certificate, err := decodeAndParseCertificate(cert, key)
		if err != nil {
			vc.log.Error(err)
			invalidFormatCerts += key + ", "
			continue
		}

		// this is for checking that our certificates are of x509 V3 format or not.
		if certificate.SignatureAlgorithm == x509.UnknownSignatureAlgorithm || certificate.Version != constants.X509_VERSION {
			vc.log.Debugf("%s certificate is not in x509 V3 format", key)
			invalidFormatCerts += key + ", "
			continue
		}
	}

	if invalidFormatCerts == "" {
		vc.log.Debug("All Certificates are in x509 V3 format")
		return createCheck(constants.CERTIFICATE_FORMAT_TITLE, true, constants.CERTIFICATE_FORMAT_SUCCESS_MESSAGE, "", "")
	}

	invalidFormatCerts = strings.TrimSuffix(invalidFormatCerts, ", ")
	return createCheck(constants.CERTIFICATE_FORMAT_TITLE, false, "", fmt.Sprintf(constants.CERTIFICATE_FORMAT_ERROR_MESSAGE, invalidFormatCerts), constants.CERTIFICATE_FORMAT_RESOLUTION_MESSAGE)
}

func (vc *ValidateCertificateService) validateKeyFormat(privateKeys map[string]string, keys []string) models.Checks {
	vc.log.Debug("Validating Keys Format...")
	invalidFormatKeys := ""

	for _, key := range keys {
		cert := privateKeys[key]
		block, _ := pem.Decode([]byte(cert))
		if block == nil {
			vc.log.Errorf("Failed to decode %s", key)
			invalidFormatKeys += key + ", "
			continue
		}

		// this is for checking that our private keys are of PKCS8 format or not.

	}

	if invalidFormatKeys == "" {
		vc.log.Debug("Keys are in PKCS8 format")
		return createCheck(constants.KEY_FORMAT_TITLE, true, constants.KEY_FORMAT_SUCCESS_MESSAGE, "", "")
	}

	invalidFormatKeys = strings.TrimSuffix(invalidFormatKeys, ", ")
	return createCheck(constants.KEY_FORMAT_TITLE, false, "", fmt.Sprintf(constants.KEY_FORMAT_ERROR_MESSAGE, invalidFormatKeys), fmt.Sprintf(constants.KEY_FORMAT_RESOLUTION_MESSAGE, invalidFormatKeys))
}

func (vc *ValidateCertificateService) validateCertificateAlgorithm(certificates map[string]string, keys []string) models.Checks {
	vc.log.Debug("Validating Certificates Hashing Algorithm...")
	invalidAlgoCerts := ""

	for _, key := range keys {
		cert := certificates[key]
		certificate, err := decodeAndParseCertificate(cert, key)
		if err != nil {
			vc.log.Error(err)
			invalidAlgoCerts += key + ", "
			continue
		}

		// this is for checking that our certificates are hashed using either of PBE-SHA1-3DES,
		// RSA (2048), SHA-256 algorithms or not.
		signatureAlgorithm := certificate.SignatureAlgorithm
		if (signatureAlgorithm != x509.SHA256WithRSA && signatureAlgorithm != x509.ECDSAWithSHA256) &&
			(certificate.PublicKeyAlgorithm != x509.RSA || certificate.PublicKey.(*rsa.PublicKey).N.BitLen() != 2048) &&
			signatureAlgorithm.String() != "PBE-SHA1-3DES" {
			vc.log.Debugf("%s certificate is not hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms", key)
			invalidAlgoCerts += key + ", "
			continue
		}
	}

	if invalidAlgoCerts == "" {
		vc.log.Debug("All certificates are hashed using either of PBE-SHA1-3DES, RSA (2048), SHA-256 algorithms")
		return createCheck(constants.CERTIFICATE_ALGORITHM_TITLE, true, constants.CERTIFICATE_ALGORITHM_SUCCESS_MESSAGE, "", "")
	}

	invalidAlgoCerts = strings.TrimSuffix(invalidAlgoCerts, ", ")
	return createCheck(constants.CERTIFICATE_ALGORITHM_TITLE, false, "", fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_ERROR_MESSAGE, invalidAlgoCerts), fmt.Sprintf(constants.CERTIFICATE_ALGORITHM_RESOLUTION_MESSAGE, invalidAlgoCerts))
}

func (vc *ValidateCertificateService) CertificateValidation(req models.CertificateCheckRequest) models.CertificateCheckResponse {
	var response = models.CertificateCheckResponse{}
	// certKeys are required to maintain the order of correct response messages.
	var certKeys []string
	// certificates map is used for storing Root Certificate, Node Certificate and if it is a case of opensearch then Admin Certificate.
	certificates := make(map[string]string)

	// privateKeys map is used for storing Private Key and if it is a case of opensearch then Admin Private Key.
	privateKeys := make(map[string]string)

	if strings.TrimSpace(req.RootCertificate) != "" {
		certificates[constants.ROOT] = req.RootCertificate
		certKeys = append(certKeys, constants.ROOT)
	}
	certificates[constants.NODE] = req.NodeCertificate
	certKeys = append(certKeys, constants.NODE)

	//keys array are required to maintain the order of correct response messages.
	keys := []string{constants.NODE_KEY}
	privateKeys[constants.NODE_KEY] = req.PrivateKey

	if strings.TrimSpace(req.AdminCertificate) != "" && strings.TrimSpace(req.AdminPrivateKey) != "" {
		certificates[constants.ADMIN] = req.AdminCertificate
		privateKeys[constants.ADMIN_KEY] = req.AdminPrivateKey
		certKeys = append(certKeys, constants.ADMIN)
		keys = append(keys, constants.ADMIN_KEY)
	}

	response.Checks = append(response.Checks, vc.validateCertificateExpiry(certificates, certKeys))
	response.Checks = append(response.Checks, vc.validateCertificateFormat(certificates, certKeys))
	response.Checks = append(response.Checks, vc.validateKeyFormat(privateKeys, keys))
	response.Checks = append(response.Checks, vc.validateCertificateAlgorithm(certificates, certKeys))

	response.Passed = true
	for _, k := range response.Checks {
		if !k.Passed {
			response.Passed = false
			break
		}
	}
	return response
}
