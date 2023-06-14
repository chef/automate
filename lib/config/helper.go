package config

import (
	"container/list"
	"crypto/x509"
	"encoding/pem"
	"errors"
	"fmt"
	"net"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/chef/automate/components/local-user-service/password"
)

type keydetails struct {
	key      string
	certtype string
	svc      string
}

func validateNumberField(value string, fieldName string, isRequired bool) error {
	if !isRequired && len(value) < 1 {
		return nil
	}

	if len(value) < 1 {
		return fmt.Errorf(EMPTY_VALUE, fieldName)
	}

	if _, err := strconv.Atoi(value); err != nil {
		return fmt.Errorf(INVALID_FIELD_VALUE, fieldName, value)
	}

	return nil
}

func validateStringBasedBoolean(value string, fieldName string, isRequired bool) error {
	if !isRequired && value == "" {
		return nil
	}
	// Check if the string represents a valid boolean value
	_, err := strconv.ParseBool(value)
	if err != nil {
		return fmt.Errorf(INVALID_FIELD_VALUE, fieldName, value)
	}
	return nil
}

func validateRequiredPathField(value string, fieldName string) error {
	if len(value) == 0 {
		return fmt.Errorf(INVALID_EMPTY_VALUE, fieldName)
	}
	if _, err := os.Stat(value); err != nil {
		return fmt.Errorf("invalid %s: %s (%v)", fieldName, value, err)
	}
	return nil
}

func validateRequiredString(value string, fieldName string, expectedValues ...string) error {
	if len(value) < 1 {
		return fmt.Errorf(INVALID_EMPTY_VALUE, fieldName)
	}
	if len(expectedValues) > 0 {
		if !contains(expectedValues, value) {
			expectedValuesStr := strings.Join(expectedValues, ", ")
			return fmt.Errorf("invalid value '%s' for field '%s'. Expected values are: %s", value, fieldName, expectedValuesStr)
		}
	}
	return nil
}

func contains(slice []string, value string) bool {
	for _, v := range slice {
		if v == value {
			return true
		}
	}
	return false
}

func validatePort(value string, fieldName string, isRequired bool) error {
	if !isRequired && len(value) < 1 {
		return nil
	}
	portNumber, err := strconv.Atoi(value)
	if err != nil {
		return fmt.Errorf(INVALID_PORT_NUMBER, value, fieldName)
	}
	// Port number must be between 1 and 65535 (inclusive)
	if portNumber < 1 || portNumber > 65535 {
		return fmt.Errorf(INVALID_PORT_NUMBER, value, fieldName)
	}
	return nil
}

func validateRequiredStringListField(value []string, fieldName string, minNumber ...int) error {
	if len(value) < 1 {
		return fmt.Errorf(INVALID_EMPTY_VALUE, fieldName)
	}

	if len(minNumber) > 0 && len(value) < minNumber[0] {
		return fmt.Errorf("minimum number of %s required is %d", fieldName, minNumber[0])
	}

	return nil
}

func getSingleErrorFromList(errorList *list.List) error {
	if errorList.Len() > 0 {
		var errorMsgs []string
		for e := errorList.Front(); e != nil; e = e.Next() {
			switch value := e.Value.(type) {
			case error:
				errorMsgs = append(errorMsgs, value.Error())
			case string:
				errorMsgs = append(errorMsgs, value)
			default:
				errorMsgs = append(errorMsgs, fmt.Sprintf("unknown error type: %v", value))
			}
		}
		return fmt.Errorf(strings.Join(errorMsgs, "\n"))
	}
	return nil
}

func commonFqdnValidation(value string, keyName string) error {
	if len(value) < 1 {
		return fmt.Errorf("invalid or empty URL: " + keyName)
	}
	if strings.Contains(value, " ") {
		return fmt.Errorf("domain name cannot contain spaces: " + keyName)
	}
	// Check for "http://" or "https://" in the URL
	if strings.HasPrefix(value, "http://") || strings.HasPrefix(value, "https://") {
		return fmt.Errorf("url should not include the protocol (http:// or https://): " + keyName)
	}
	return nil
}

func validateFQDN(value string, keyName string) error {

	if err := commonFqdnValidation(value, keyName); err != nil {
		return err
	}
	// Regular expression to validate FQDN with or without port number
	regex := `^(?:[a-zA-Z0-9](?:[a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z0-9](?:[a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?(?::\d+)?$`
	match, _ := regexp.MatchString(regex, value)
	if !match {
		return fmt.Errorf("invalid URL format: " + keyName)
	}
	return nil
}

func validateUrlWithPort(value string, keyName string) error {

	if err := commonFqdnValidation(value, keyName); err != nil {
		return err
	}
	// Regular expression to validate Url with port number
	regex := `^[a-zA-Z0-9-]+(\.[a-zA-Z0-9-]+)*:\d{2,5}$`
	match, _ := regexp.MatchString(regex, value)
	if !match {
		return fmt.Errorf("invalid URL format: " + keyName)
	}
	return nil

}

func checkForValidS3Bucket(c *HaDeployConfig) error {
	if c.Architecture.Aws.BackupConfig == "s3" && len(strings.TrimSpace(c.Architecture.Aws.S3BucketName)) < 1 {
		return fmt.Errorf("invalid or empty s3_bucketName")
	}
	return nil
}

func validateAutomateAdminPassword(automateSettings *ConfigAutomateSettings) error {
	if len(automateSettings.AdminPassword) > 0 {
		val, err := password.NewValidator()
		if err != nil {
			return fmt.Errorf(err.Error())
		}
		passvalErr := val.Validate(automateSettings.AdminPassword)
		if passvalErr != nil {
			return fmt.Errorf(passvalErr.Error())
		}
	}
	return nil
}

func validateAutomateCerts(automateSettings *ConfigAutomateSettings) error {
	if len(strings.TrimSpace(automateSettings.RootCA)) < 1 ||
		len(strings.TrimSpace(automateSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(automateSettings.PublicKey)) < 1 {
		return fmt.Errorf("automate root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false")
	}

	return checkCertValid([]keydetails{
		{key: automateSettings.RootCA, certtype: ROOT_CA, svc: AUTOMATE},
		{key: automateSettings.PrivateKey, certtype: PRIVATE_KEY, svc: AUTOMATE},
		{key: automateSettings.PublicKey, certtype: PUBLIC_KEY, svc: AUTOMATE},
	})
}

func validateChefServerCerts(chefServerSettings *ConfigSettings) error {
	if len(strings.TrimSpace(chefServerSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(chefServerSettings.PublicKey)) < 1 {
		return fmt.Errorf("chefServer public_key and/or private_key are missing. Otherwise set enable_custom_certs to false")
	}

	return checkCertValid([]keydetails{
		{key: chefServerSettings.PrivateKey, certtype: PRIVATE_KEY, svc: CHEFSERVER},
		{key: chefServerSettings.PublicKey, certtype: PUBLIC_KEY, svc: CHEFSERVER},
	})
}

func checkCertValid(keys []keydetails) error {
	errorList := list.New()
	for _, el := range keys {
		block, _ := pem.Decode([]byte(el.key))
		if block == nil {
			errorList.PushBack("invalid format. Failed to decode " + el.certtype + " for " + el.svc)
			continue
		}
		switch el.certtype {
		case ROOT_CA:
			err := validateRootCACertificate(block.Bytes, el.svc)
			if err != nil {
				errorList.PushBack(err.Error())
			}
		default:
			continue
		}
	}
	return getSingleErrorFromList(errorList)
}

func validateRootCACertificate(certBytes []byte, svc string) error {
	rootCA, err := x509.ParseCertificate(certBytes)
	if err != nil {
		return fmt.Errorf("failed to parse root_ca for %s: %v", svc, err)
	}
	if time.Now().After(rootCA.NotAfter) {
		return fmt.Errorf("root_ca for %s has expired", svc)
	}
	return nil
}

func validatePostgresqlCerts(postgresqlSettings *ConfigSettings) error {
	if len(strings.TrimSpace(postgresqlSettings.RootCA)) < 1 ||
		len(strings.TrimSpace(postgresqlSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(postgresqlSettings.PublicKey)) < 1 {
		return fmt.Errorf("postgresql root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false")
	}

	return checkCertValid([]keydetails{
		{key: postgresqlSettings.RootCA, certtype: ROOT_CA, svc: POSTGRESQL},
		{key: postgresqlSettings.PrivateKey, certtype: PRIVATE_KEY, svc: POSTGRESQL},
		{key: postgresqlSettings.PublicKey, certtype: PUBLIC_KEY, svc: POSTGRESQL},
	})
}

func validateOpensearchCerts(opensearchSettings *ConfigOpensearchSettings) error {
	if len(strings.TrimSpace(opensearchSettings.RootCA)) < 1 ||
		len(strings.TrimSpace(opensearchSettings.AdminKey)) < 1 ||
		len(strings.TrimSpace(opensearchSettings.AdminCert)) < 1 ||
		len(strings.TrimSpace(opensearchSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(opensearchSettings.PublicKey)) < 1 {
		return fmt.Errorf("opensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false")
	}

	return checkCertValid([]keydetails{
		{key: opensearchSettings.RootCA, certtype: ROOT_CA, svc: OPENSEARCH},
		{key: opensearchSettings.AdminKey, certtype: ADMIN_KEY, svc: OPENSEARCH},
		{key: opensearchSettings.AdminCert, certtype: ADMIN_CERT, svc: OPENSEARCH},
		{key: opensearchSettings.PrivateKey, certtype: PRIVATE_KEY, svc: OPENSEARCH},
		{key: opensearchSettings.PublicKey, certtype: PUBLIC_KEY, svc: OPENSEARCH},
	})
}

func validateIPList(ipList []string, prefix string) error {
	errorList := list.New()
	for _, element := range ipList {
		if checkIPAddress(element) != nil {
			errorList.PushBack(prefix + " " + element + "is not valid")
		}
	}
	return getSingleErrorFromList(errorList)
}

func validateCertsByIP(certsByIpList *[]CertByIP, nodeType string) error {
	if certsByIpList == nil {
		return nil
	}
	errorList := list.New()
	for _, element := range *certsByIpList {
		if len(strings.TrimSpace(element.IP)) < 1 ||
			len(strings.TrimSpace(element.PrivateKey)) < 1 ||
			len(strings.TrimSpace(element.PublicKey)) < 1 {
			return fmt.Errorf(nodeType + "public_key and/or private_key are missing in certs_by_ip. Otherwise set enable_custom_certs to false")
		}
		if checkIPAddress(element.IP) != nil {
			errorList.PushBack(nodeType + " " + element.IP + " for certs is not valid")
		}
		errorList.PushBack(checkCertValid([]keydetails{
			{key: element.PrivateKey, certtype: PRIVATE_KEY, svc: nodeType},
			{key: element.PublicKey, certtype: PUBLIC_KEY, svc: nodeType},
		}))
	}
	return getSingleErrorFromList(errorList)
}

func checkIPAddress(ip string) error {
	if net.ParseIP(ip) == nil {
		return fmt.Errorf("ip address is invalid")
	}
	return nil
}

func validateS3AWSRegion(region string) error {
	// List of AWS regions that support S3
	// Reference: https://aws.amazon.com/about-aws/global-infrastructure/regional-product-services/
	supportedRegions := []string{
		"us-east-1",
		"us-east-2",
		"us-west-1",
		"us-west-2",
		"af-south-1",
		"ap-east-1",
		"ap-south-1",
		"ap-northeast-1",
		"ap-northeast-2",
		"ap-northeast-3",
		"ap-southeast-1",
		"ap-southeast-2",
		"ca-central-1",
		"cn-north-1",
		"cn-northwest-1",
		"eu-central-1",
		"eu-west-1",
		"eu-west-2",
		"eu-south-1",
		"eu-west-3",
		"eu-north-1",
		"me-south-1",
		"sa-east-1",
	}

	// Convert the provided region and supported regions to lowercase for case-insensitive comparison
	region = strings.ToLower(region)
	for _, supportedRegion := range supportedRegions {
		if region == supportedRegion {
			return nil
		}
	}
	return errors.New("invalid AWS region for S3")
}

func validateS3Endpoint(endpoint string) error {
	if endpoint == "" {
		return errors.New("invalid or empty: endpoint")
	}
	if strings.HasPrefix(endpoint, "http://") || strings.HasPrefix(endpoint, "https://") {
		return errors.New("s3 endpoint should not include the HTTP protocol")
	}
	if !strings.HasSuffix(endpoint, ".amazonaws.com") {
		return errors.New("invalid S3 endpoint format. Endpoint should end with '.amazonaws.com'")
	}
	return nil
}
