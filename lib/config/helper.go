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

func validateRequiredNumberField(value string, fieldName string, errorList *list.List) {
	if len(value) > 0 {
		if _, err := strconv.Atoi(value); err != nil {
			errorList.PushBack(fmt.Sprintf("Invalid %s: %s", fieldName, value))
		}
	} else {
		errorList.PushBack(fmt.Sprintf(INVALID_EMPTY_VALUE, fieldName))
	}
}
func validateNumberField(value string, fieldName string, errorList *list.List) {
	if len(value) > 0 {
		if _, err := strconv.Atoi(value); err != nil {
			errorList.PushBack(fmt.Sprintf("Invalid %s: %s", fieldName, value))
		}
	}
}

func validateRequiredBooleanField(value interface{}, fieldName string, errorList *list.List) {
	_, ok := value.(bool)
	if !ok {
		errorList.PushBack(fmt.Sprintf("Invalid %s: %s", fieldName, value))
	}
}

func validateStringBasedBoolean(value interface{}, fieldName string, errorList *list.List) {
	switch v := value.(type) {
	case string:
		// Check if the string represents a valid boolean value
		_, err := strconv.ParseBool(v)
		if err != nil {
			errorList.PushBack(fmt.Sprintf("Invalid %s: %s", fieldName, value))
		}
	default:
		errorList.PushBack(fmt.Sprintf("Invalid %s: %v", fieldName, value))
	}
}

func validateRequiredPathField(value string, fieldName string, errorList *list.List) {
	if len(value) > 0 {
		if _, err := os.Stat(value); err != nil {
			if errors.Is(err, os.ErrNotExist) {
				errorList.PushBack(fmt.Sprintf("Invalid %s: %s (path does not exist)", fieldName, value))
			} else {
				errorList.PushBack(fmt.Sprintf("Invalid %s: %s (%v)", fieldName, value, err))
			}
		}
	} else {
		errorList.PushBack(fmt.Sprintf(INVALID_EMPTY_VALUE, fieldName))
	}
}

func validateRequiredStringTypeField(value interface{}, fieldName string, errorList *list.List) {
	strValue, ok := value.(string)
	if !ok || len(strings.TrimSpace(strValue)) < 1 {
		errorList.PushBack(fmt.Sprintf(INVALID_EMPTY_VALUE, fieldName))
	}
}

func validateStringTypeField(value interface{}, fieldName string, errorList *list.List) {
	if value == nil {
		return
	}
	_, ok := value.(string)
	if !ok {
		errorList.PushBack(fmt.Sprintf(INVALID_STRING_TYPE, fieldName))
	}
}

func validateRequiredStringListField(value []string, fieldName string, errorList *list.List) {
	if len(value) < 1 {
		errorList.PushBack(fmt.Sprintf(INVALID_EMPTY_VALUE, fieldName))
	}
}

func validateBackupMount(mount string, errorList *list.List) {
	if len(mount) < 1 {
		errorList.PushBack("Invalid or empty backup_mount")
	} else if mount != "/mnt/automate_backups" {
		errorList.PushBack("backup_mount has to be /mnt/automate_backups")
	}
}

func getSingleErrorFromList(errorList *list.List) error {
	if errorList.Len() > 0 {
		var errorMsgs []string
		for e := errorList.Front(); e != nil; e = e.Next() {
			errorMsgs = append(errorMsgs, e.Value.(string))
		}
		return fmt.Errorf(strings.Join(errorMsgs, "\n"))
	}
	return nil
}

func validateUrl(value string, keyName string, errorList *list.List) {
	if len(value) < 1 {
		errorList.PushBack("Invalid or empty URL: " + keyName)
		return
	}
	if strings.Contains(value, " ") {
		errorList.PushBack("Domain name cannot contain spaces: " + keyName)
		return
	}
	// Check for "http://" or "https://" in the URL
	if strings.HasPrefix(value, "http://") || strings.HasPrefix(value, "https://") {
		errorList.PushBack("URL should not include the protocol (http:// or https://): " + keyName)
		return
	}
	// Regular expression to validate URLs with or without port number
	regex := `^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])(:\d+)?$`
	match, _ := regexp.MatchString(regex, value)
	if !match {
		errorList.PushBack("Invalid URL format: " + keyName)
	}
}

func validateConfigFile(configFile string, errorList *list.List) {
	if configFile != "configs/automate.toml" {
		errorList.PushBack("invalid config_file value it has to be configs/automate.toml")
	}
}

func checkForS3BackupConfig(c *HaDeployConfig, errorList *list.List) {
	if c.Architecture.Aws.BackupConfig == "s3" {
		checkForValidS3Bucket(c.Architecture.Aws, errorList)
	}
}

func checkManagedServicesBackupConfig(c *HaDeployConfig, errorList *list.List) {
	if c.Architecture.Aws.BackupConfig != "s3" {
		errorList.PushBack("Invalid backup_config. Only 's3' is supported.")
	}
}

func checkNonManagedServicesBackupConfig(c *HaDeployConfig, errorList *list.List) {
	if c.Architecture.Aws.BackupConfig != "efs" && c.Architecture.Aws.BackupConfig != "s3" {
		errorList.PushBack("Invalid backup_config. It should be 'efs' or 's3'.")
	}
}

func checkForValidS3Bucket(configInitials *ConfigInitials, errorList *list.List) {
	if configInitials.BackupConfig == "s3" && len(strings.TrimSpace(configInitials.S3BucketName)) < 1 {
		errorList.PushBack("Invalid or empty s3_bucketName.")
	}
}

func validateAutomateAdminPassword(automateSettings *ConfigAutomateSettings, errorList *list.List) {
	if len(automateSettings.AdminPassword) > 0 {
		val, err := password.NewValidator()
		if err != nil {
			errorList.PushBack(err.Error())
		}
		passvalErr := val.Validate(automateSettings.AdminPassword)
		if passvalErr != nil {
			errorList.PushBack(passvalErr.Error())
		}
	}
}

func validateAutomateCerts(automateSettings *ConfigAutomateSettings, errorList *list.List) {
	if len(strings.TrimSpace(automateSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(automateSettings.PublicKey)) < 1 {
		errorList.PushBack("Automate public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
	}

	if len(strings.TrimSpace(automateSettings.RootCA)) > 0 {
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: automateSettings.RootCA, certtype: rootCa, svc: automate},
		}))
	}

	errorList.PushBackList(checkCertValid([]keydetails{
		{key: automateSettings.PrivateKey, certtype: privateKey, svc: automate},
		{key: automateSettings.PublicKey, certtype: publicKey, svc: automate},
	}))
}

func validateChefServerCerts(chefServerSettings *ConfigSettings, errorList *list.List) {
	if len(strings.TrimSpace(chefServerSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(chefServerSettings.PublicKey)) < 1 {
		errorList.PushBack("ChefServer root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
	}

	errorList.PushBackList(checkCertValid([]keydetails{
		{key: chefServerSettings.PrivateKey, certtype: privateKey, svc: chefServer},
		{key: chefServerSettings.PublicKey, certtype: publicKey, svc: chefServer},
	}))
}

func checkCertValid(keys []keydetails) *list.List {
	errorList := list.New()
	for _, el := range keys {
		block, _ := pem.Decode([]byte(el.key))
		if block == nil {
			errorList.PushBack("Invalid format. Failed to decode " + el.certtype + " for " + el.svc)
		} else {
			switch el.certtype {
			case rootCa:
				err := validateRootCACertificate(block.Bytes, el.svc, errorList)
				if err != nil {
					errorList.PushBack(err.Error())
				}
			default:
				errorList.PushBack("Unknown certificate type: " + el.certtype + " for " + el.svc)
			}
		}
	}
	return errorList
}

func validateRootCACertificate(certBytes []byte, svc string, errorList *list.List) error {
	rootCA, err := x509.ParseCertificate(certBytes)
	if err != nil {
		return fmt.Errorf("failed to parse root_ca for %s: %v", svc, err)
	}
	if time.Now().After(rootCA.NotAfter) {
		return fmt.Errorf("root_ca for %s has expired", svc)
	}
	return nil
}

func validatePostgresqlCerts(postgresqlSettings *ConfigSettings, errorList *list.List) {
	if len(strings.TrimSpace(postgresqlSettings.RootCA)) < 1 ||
		len(strings.TrimSpace(postgresqlSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(postgresqlSettings.PublicKey)) < 1 {
		errorList.PushBack("Postgresql root_ca and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
	}

	errorList.PushBackList(checkCertValid([]keydetails{
		{key: postgresqlSettings.RootCA, certtype: rootCa, svc: postgreSql},
		{key: postgresqlSettings.PrivateKey, certtype: privateKey, svc: postgreSql},
		{key: postgresqlSettings.PublicKey, certtype: publicKey, svc: postgreSql},
	}))
}

func validateOpensearchCerts(opensearchSettings *ConfigOpensearchSettings, errorList *list.List) {
	if len(strings.TrimSpace(opensearchSettings.RootCA)) < 1 ||
		len(strings.TrimSpace(opensearchSettings.AdminKey)) < 1 ||
		len(strings.TrimSpace(opensearchSettings.AdminCert)) < 1 ||
		len(strings.TrimSpace(opensearchSettings.PrivateKey)) < 1 ||
		len(strings.TrimSpace(opensearchSettings.PublicKey)) < 1 {
		errorList.PushBack("Opensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Otherwise set enable_custom_certs to false.")
	}

	errorList.PushBackList(checkCertValid([]keydetails{
		{key: opensearchSettings.RootCA, certtype: rootCa, svc: openSearch},
		{key: opensearchSettings.AdminKey, certtype: adminKey, svc: openSearch},
		{key: opensearchSettings.AdminCert, certtype: adminCert, svc: openSearch},
		{key: opensearchSettings.PrivateKey, certtype: privateKey, svc: openSearch},
		{key: opensearchSettings.PublicKey, certtype: publicKey, svc: openSearch},
	}))
}

func isExternalDb(externalDbSettings *ExternalDBSettings) bool {
	return externalDbSettings.Type == "aws" || externalDbSettings.Type == "self-managed"
}

// func isExternalDbAwsManaged(externalDbSettings *ExternalDBSettings) bool {
// 	return externalDbSettings.Type == "aws"
// }

func isExternalDbSelfManaged(externalDbSettings *ExternalDBSettings) bool {
	return externalDbSettings.Type == "self-managed"
}

// func isSetupAwsManagedServices(awsSettings *ConfigAwsSettings) bool {
// 	return awsSettings.SetupManagedServices
// }

func validateIPList(ipList []string, prefix string, errorList *list.List) {
	for _, element := range ipList {
		if checkIPAddress(element) != nil {
			errorList.PushBack(prefix + " " + element + "is not valid")
		}
	}
}

func validateCertsByIP(certsByIpList []CertByIP, nodeType string, errorList *list.List) {
	for _, element := range certsByIpList {
		if checkIPAddress(element.IP) != nil {
			errorList.PushBack(nodeType + " " + element.IP + " for certs is not valid")
		}
	}
}

func checkIPAddress(ip string) error {
	if net.ParseIP(ip) == nil {
		return errors.New("ip Address is invalid")
	}
	return nil
}
