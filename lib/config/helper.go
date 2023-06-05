package config

import (
	"container/list"
	"crypto/x509"
	"encoding/pem"
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

func validateRequiredNumberField(value string, fieldName string) error {
	if len(value) == 0 {
		return fmt.Errorf(fmt.Sprintf(INVALID_EMPTY_VALUE, fieldName))

	}
	if _, err := strconv.Atoi(value); err != nil {
		return fmt.Errorf(fmt.Sprintf(INVALID_FIELD_VALUE, fieldName, value))
	}
	return nil
}

func validateNumberField(value string, fieldName string) error {
	if len(value) > 0 {
		if _, err := strconv.Atoi(value); err != nil {
			return fmt.Errorf(fmt.Sprintf(INVALID_FIELD_VALUE, fieldName, value))
		}
	}
	return nil
}

func validateRequiredBooleanField(value interface{}, fieldName string) error {
	_, ok := value.(bool)
	if !ok {
		return fmt.Errorf(fmt.Sprintf(INVALID_FIELD_VALUE, fieldName, value))
	}
	return nil
}

func validateStringBasedBoolean(value interface{}, fieldName string) error {
	switch v := value.(type) {
	case string:
		// Check if the string represents a valid boolean value
		_, err := strconv.ParseBool(v)
		if err != nil {
			return fmt.Errorf(fmt.Sprintf(INVALID_FIELD_VALUE, fieldName, value))
		}
	default:
		return fmt.Errorf(fmt.Sprintf("Invalid %s: %v", fieldName, value))
	}
	return nil
}

func validateRequiredPathField(value string, fieldName string) error {
	if len(value) == 0 {
		return fmt.Errorf(fmt.Sprintf(INVALID_EMPTY_VALUE, fieldName))
	}
	if _, err := os.Stat(value); err != nil {
		return fmt.Errorf(fmt.Sprintf("Invalid %s: %s (%v)", fieldName, value, err))
	}
	return nil
}

func validateRequiredStringTypeField(value string, fieldName string, expectedValues ...string) error {
	if len(strings.TrimSpace(value)) < 1 {
		return fmt.Errorf(fmt.Sprintf(INVALID_EMPTY_VALUE, fieldName))
	}
	if len(expectedValues) > 0 {
		if !contains(expectedValues, value) {
			expectedValuesStr := strings.Join(expectedValues, ", ")
			return fmt.Errorf(fmt.Sprintf("Invalid value '%s' for field '%s'. Expected values are: %s", value, fieldName, expectedValuesStr))
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

func validateStringTypeField(value interface{}, fieldName string) error {
	if value == nil {
		return nil
	}
	_, ok := value.(string)
	if !ok {
		return fmt.Errorf(fmt.Sprintf(INVALID_STRING_TYPE, fieldName))
	}
	return nil
}

func validateRequiredStringListField(value []string, fieldName string) error {
	if len(value) < 1 {
		return fmt.Errorf(fmt.Sprintf(INVALID_EMPTY_VALUE, fieldName))
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
				errorMsgs = append(errorMsgs, fmt.Sprintf("Unknown error type: %v", value))
			}
		}
		return fmt.Errorf(strings.Join(errorMsgs, "\n"))
	}
	return nil
}

func validateUrl(value string, keyName string) error {
	if len(value) < 1 {
		return fmt.Errorf("Invalid or empty URL: " + keyName)
	}
	if strings.Contains(value, " ") {
		return fmt.Errorf("Domain name cannot contain spaces: " + keyName)
	}
	// Check for "http://" or "https://" in the URL
	if strings.HasPrefix(value, "http://") || strings.HasPrefix(value, "https://") {
		return fmt.Errorf("URL should not include the protocol (http:// or https://): " + keyName)
	}
	// Regular expression to validate URLs with or without port number
	regex := `^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])(:\d+)?$`
	match, _ := regexp.MatchString(regex, value)
	if !match {
		return fmt.Errorf("Invalid URL format: " + keyName)
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
		{key: automateSettings.RootCA, certtype: rootCa, svc: automate},
		{key: automateSettings.PrivateKey, certtype: privateKey, svc: automate},
		{key: automateSettings.PublicKey, certtype: publicKey, svc: automate},
	})
}

func validateChefServerCerts(chefServerSettings *ConfigSettings) error {
	if len(strings.TrimSpace(chefServerSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(chefServerSettings.PublicKey)) < 1 {
		return fmt.Errorf("chefServer public_key and/or private_key are missing. Otherwise set enable_custom_certs to false")
	}

	return checkCertValid([]keydetails{
		{key: chefServerSettings.PrivateKey, certtype: privateKey, svc: chefServer},
		{key: chefServerSettings.PublicKey, certtype: publicKey, svc: chefServer},
	})
}

func checkCertValid(keys []keydetails) error {
	errorList := list.New()
	for _, el := range keys {
		block, _ := pem.Decode([]byte(el.key))
		if block == nil {
			errorList.PushBack("Invalid format. Failed to decode " + el.certtype + " for " + el.svc)
			continue
		}
		switch el.certtype {
		case rootCa:
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
		{key: postgresqlSettings.RootCA, certtype: rootCa, svc: postgreSql},
		{key: postgresqlSettings.PrivateKey, certtype: privateKey, svc: postgreSql},
		{key: postgresqlSettings.PublicKey, certtype: publicKey, svc: postgreSql},
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
		{key: opensearchSettings.RootCA, certtype: rootCa, svc: openSearch},
		{key: opensearchSettings.AdminKey, certtype: adminKey, svc: openSearch},
		{key: opensearchSettings.AdminCert, certtype: adminCert, svc: openSearch},
		{key: opensearchSettings.PrivateKey, certtype: privateKey, svc: openSearch},
		{key: opensearchSettings.PublicKey, certtype: publicKey, svc: openSearch},
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
			return fmt.Errorf(nodeType + "root_ca and/or public_key and/or private_key are missing in certs_by_ip. Otherwise set enable_custom_certs to false")
		}
		if checkIPAddress(element.IP) != nil {
			errorList.PushBack(nodeType + " " + element.IP + " for certs is not valid")
		}
		errorList.PushBack(checkCertValid([]keydetails{
			{key: element.PrivateKey, certtype: privateKey, svc: nodeType},
			{key: element.PublicKey, certtype: publicKey, svc: nodeType},
		}))
	}
	return getSingleErrorFromList(errorList)
}

func checkIPAddress(ip string) error {
	if net.ParseIP(ip) == nil {
		return fmt.Errorf("ip Address is invalid")
	}
	return nil
}
