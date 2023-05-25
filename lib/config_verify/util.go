package config_verify

import (
	"container/list"
	"crypto/x509"
	"encoding/pem"
	"errors"
	"net"
	"strings"
	"time"

	"github.com/chef/automate/lib/config_parser"
	"github.com/chef/automate/lib/stringutils"
)

const (
	FOR_CERTS = " for certs "
)

func validateChannel(channel string) (bool, string) {
	return validateOneOf("Channel", channel, []string{"current", "acceptance", "dev"})
}

func validateUpgradeStrategy(strategy string) (bool, string) {
	return validateOneOf("Upgrade strategy", strategy, []string{"none", "at-once"})
}

func validatePackageCleanupMode(mode string) (bool, string) {
	return validateOneOf("Package cleanup mode", mode, []string{"conservative", "aggressive", "disabled"})
}

func validateOneOf(msgPrefix string, input string, allowedValues []string) (bool, string) {
	valid := stringutils.SliceContains(allowedValues, input)
	if !valid {
		return valid, msgPrefix + " must be one of: " + strings.Join(allowedValues, ", ")
	}
	return valid, ""
}

func checkIPAddress(ip string) error {
	if net.ParseIP(ip) == nil {
		return errors.New("ip Address is invalid")
	}
	return nil
}

func extractIPsFromCertsByIP(certsByIp []config_parser.CertByIP) []string {
	ips := []string{}
	for _, el := range certsByIp {
		ips = append(ips, el.IP)
	}
	return ips
}

func checkCertValid(keys []keydetails) *list.List {
	errorList := list.New()
	for _, el := range keys {
		block, _ := pem.Decode([]byte(el.key))
		if block == nil {
			errorList.PushBack("Invalid format. Failed to decode " + el.certtype + " for " + el.svc)
		} else if el.certtype == rootCa {
			rootCA, err := x509.ParseCertificate(block.Bytes)
			if err != nil {
				errorList.PushBack("Failed to parse " + el.certtype + " for " + el.svc)
			} else {
				if time.Now().After(rootCA.NotAfter) {
					errorList.PushBack(el.certtype + " for " + el.svc + " certificate has expired.")
				}
			}
		}
	}
	return errorList
}

func validateCerts(config *config_parser.HAOnPremConfigToml) *list.List {
	errorList := list.New()

	if config.Automate.Config.EnableCustomCerts {
		validateAutomateCerts(config, errorList)
	}

	if config.ChefServer.Config.EnableCustomCerts {
		validateChefServerCerts(config, errorList)
	}

	if config.Postgresql.Config.EnableCustomCerts {
		validatePostgresqlCerts(config, errorList)
	}

	if config.Opensearch.Config.EnableCustomCerts {
		validateOpensearchCerts(config, errorList)
	}

	return errorList
}

func validateAutomateCerts(config *config_parser.HAOnPremConfigToml, errorList *list.List) {
	if len(config.Automate.Config.CertsByIP) > 0 {
		if len(strings.TrimSpace(config.Automate.Config.RootCA)) > 0 {
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config.Automate.Config.RootCA, certtype: rootCa, svc: automate},
			}))
		}

		if !stringutils.SubSlice(config.ExistingInfra.Config.AutomatePrivateIps, extractIPsFromCertsByIP(config.Automate.Config.CertsByIP)) {
			errorList.PushBack("Missing certificates for some automate private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(config.ExistingInfra.Config.AutomatePrivateIps, ", "))
		}

		for _, a2Node := range config.Automate.Config.CertsByIP {
			if len(strings.TrimSpace(a2Node.IP)) < 1 ||
				len(strings.TrimSpace(a2Node.PrivateKey)) < 1 ||
				len(strings.TrimSpace(a2Node.PublicKey)) < 1 {
				errorList.PushBack("Field certs_by_ip for Automate requires ip, private_key and public_key. Some of them are missing.")
			}

			errorList.PushBackList(checkCertValid([]keydetails{
				{key: a2Node.PrivateKey, certtype: privateKey, svc: automate + " cert_by_ip for ip " + a2Node.IP},
				{key: a2Node.PublicKey, certtype: publicKey, svc: automate + " cert_by_ip for ip " + a2Node.IP},
			}))
		}
	} else {
		if len(strings.TrimSpace(config.Automate.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(config.Automate.Config.PublicKey)) < 1 {
			errorList.PushBack("Automate public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
		}

		if len(strings.TrimSpace(config.Automate.Config.RootCA)) > 0 {
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config.Automate.Config.RootCA, certtype: rootCa, svc: automate},
			}))
		}

		errorList.PushBackList(checkCertValid([]keydetails{
			{key: config.Automate.Config.PrivateKey, certtype: privateKey, svc: automate},
			{key: config.Automate.Config.PublicKey, certtype: publicKey, svc: automate},
		}))
	}
}

func validateChefServerCerts(config *config_parser.HAOnPremConfigToml, errorList *list.List) {
	if len(config.ChefServer.Config.CertsByIP) > 0 {
		if !stringutils.SubSlice(config.ExistingInfra.Config.ChefServerPrivateIps, extractIPsFromCertsByIP(config.ChefServer.Config.CertsByIP)) {
			errorList.PushBack("Missing certificates for some ChefServer private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(config.ExistingInfra.Config.ChefServerPrivateIps, ", "))
		}
		// check if all the certs are valid for given IPs
		for _, csNode := range config.ChefServer.Config.CertsByIP {
			if len(strings.TrimSpace(csNode.IP)) < 1 ||
				len(strings.TrimSpace(csNode.PrivateKey)) < 1 ||
				len(strings.TrimSpace(csNode.PublicKey)) < 1 {
				errorList.PushBack("Field certs_by_ip for chef_server requires ip, private_key and public_key. Some of them are missing.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: csNode.PrivateKey, certtype: privateKey, svc: chefServer + " cert_by_ip for ip " + csNode.IP},
				{key: csNode.PublicKey, certtype: publicKey, svc: chefServer + " cert_by_ip for ip " + csNode.IP},
			}))
		}
	} else {
		// check if all the default certs are given
		if len(strings.TrimSpace(config.ChefServer.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(config.ChefServer.Config.PublicKey)) < 1 {
			errorList.PushBack("ChefServer public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: config.ChefServer.Config.PrivateKey, certtype: privateKey, svc: chefServer},
			{key: config.ChefServer.Config.PublicKey, certtype: publicKey, svc: chefServer},
		}))
	}
}

func validateAWSBackupConfig(config *config_parser.HAAwsConfigToml, errorList *list.List) {
	if config.Aws.Config.SetupManagedServices {
		if config.Architecture.ConfigInitials.BackupConfig != "s3" {
			errorList.PushBack("Invalid backup_config. Only 's3' is supported.")
		} else {
			checkForValidS3Bucket(config, errorList)
		}
	} else {
		if config.Architecture.ConfigInitials.BackupConfig != "efs" && config.Architecture.ConfigInitials.BackupConfig != "s3" {
			errorList.PushBack("Invalid backup_config. It should be 'efs' or 's3'.")
		}
	}
}

func checkForValidS3Bucket(config *config_parser.HAAwsConfigToml, errorList *list.List) {
	if config.Architecture.ConfigInitials.BackupConfig == "s3" && len(strings.TrimSpace(config.Architecture.ConfigInitials.S3BucketName)) < 1 {
		errorList.PushBack("Invalid or empty s3_bucketName.")
	}
}

func validatePostgresqlCerts(config *config_parser.HAOnPremConfigToml, errorList *list.List) {
	configOnpremPostgres := config.Postgresql
	if len(configOnpremPostgres.Config.CertsByIP) > 0 {
		if len(strings.TrimSpace(configOnpremPostgres.Config.RootCA)) < 1 {
			errorList.PushBack("Postgresql root_ca is missing. Set custom_certs_enabled to false to continue without custom certificates.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: configOnpremPostgres.Config.RootCA, certtype: rootCa, svc: postgreSql},
		}))
		if !stringutils.SubSlice(config.ExistingInfra.Config.PostgresqlPrivateIps, extractIPsFromCertsByIP(configOnpremPostgres.Config.CertsByIP)) {
			errorList.PushBack("Missing certificates for some Postgresql private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(config.ExistingInfra.Config.PostgresqlPrivateIps, ", "))
		}
		// check if all the certs are valid for given IPs
		for _, pgNode := range configOnpremPostgres.Config.CertsByIP {
			if len(strings.TrimSpace(pgNode.IP)) < 1 ||
				len(strings.TrimSpace(pgNode.PrivateKey)) < 1 ||
				len(strings.TrimSpace(pgNode.PublicKey)) < 1 {
				errorList.PushBack("Field certs_by_ip for postgresql requires ip, private_key and public_key. Some of them are missing.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: pgNode.PrivateKey, certtype: privateKey, svc: postgreSql + " cert_by_ip for ip " + pgNode.IP},
				{key: pgNode.PublicKey, certtype: publicKey, svc: postgreSql + " cert_by_ip for ip " + pgNode.IP},
			}))
		}
	} else {
		// check if all the default certs are given
		if len(strings.TrimSpace(configOnpremPostgres.Config.RootCA)) < 1 ||
			len(strings.TrimSpace(configOnpremPostgres.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(configOnpremPostgres.Config.PublicKey)) < 1 {
			errorList.PushBack("Postgresql root_ca and/or public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: configOnpremPostgres.Config.RootCA, certtype: rootCa, svc: postgreSql},
			{key: configOnpremPostgres.Config.PrivateKey, certtype: privateKey, svc: postgreSql},
			{key: configOnpremPostgres.Config.PublicKey, certtype: publicKey, svc: postgreSql},
		}))
	}
}

func validateOpensearchCerts(config *config_parser.HAOnPremConfigToml, errorList *list.List) {
	configOnpremOpensearch := config.Opensearch
	if len(configOnpremOpensearch.Config.CertsByIP) > 0 {
		if len(strings.TrimSpace(configOnpremOpensearch.Config.RootCA)) < 1 ||
			len(strings.TrimSpace(configOnpremOpensearch.Config.AdminKey)) < 1 ||
			len(strings.TrimSpace(configOnpremOpensearch.Config.AdminCert)) < 1 {
			errorList.PushBack("Opensearch root_ca, admin_key or admin_cert is missing. Set custom_certs_enabled to false to continue without custom certificates.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: configOnpremOpensearch.Config.RootCA, certtype: rootCa, svc: openSearch},
			{key: configOnpremOpensearch.Config.AdminKey, certtype: adminKey, svc: openSearch},
			{key: configOnpremOpensearch.Config.AdminCert, certtype: adminCert, svc: openSearch},
		}))
		if !stringutils.SubSlice(config.ExistingInfra.Config.OpensearchPrivateIps, extractIPsFromCertsByIP(configOnpremOpensearch.Config.CertsByIP)) {
			errorList.PushBack("Missing certificates for some Opensearch private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(config.ExistingInfra.Config.OpensearchPrivateIps, ", "))
		}
		// check if all the certs are valid for given IPs
		for _, osNode := range config.Opensearch.Config.CertsByIP {
			if len(strings.TrimSpace(osNode.IP)) < 1 ||
				len(strings.TrimSpace(osNode.PrivateKey)) < 1 ||
				len(strings.TrimSpace(osNode.PublicKey)) < 1 {
				errorList.PushBack("Field certs_by_ip for opensearch requires ip, private_key and public_key. Some of them are missing.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: osNode.PrivateKey, certtype: privateKey, svc: openSearch + " cert_by_ip for ip " + osNode.IP},
				{key: osNode.PublicKey, certtype: publicKey, svc: openSearch + " cert_by_ip for ip " + osNode.IP},
			}))
		}
	} else {
		// check if all the default certs are given
		if len(strings.TrimSpace(configOnpremOpensearch.Config.RootCA)) < 1 ||
			len(strings.TrimSpace(configOnpremOpensearch.Config.AdminKey)) < 1 ||
			len(strings.TrimSpace(configOnpremOpensearch.Config.AdminCert)) < 1 ||
			len(strings.TrimSpace(configOnpremOpensearch.Config.PrivateKey)) < 1 ||
			len(strings.TrimSpace(configOnpremOpensearch.Config.PublicKey)) < 1 {
			errorList.PushBack("Opensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
		}
		errorList.PushBackList(checkCertValid([]keydetails{
			{key: configOnpremOpensearch.Config.RootCA, certtype: rootCa, svc: openSearch},
			{key: configOnpremOpensearch.Config.AdminKey, certtype: adminKey, svc: openSearch},
			{key: configOnpremOpensearch.Config.AdminCert, certtype: adminCert, svc: openSearch},
			{key: configOnpremOpensearch.Config.PrivateKey, certtype: privateKey, svc: openSearch},
			{key: configOnpremOpensearch.Config.PublicKey, certtype: publicKey, svc: openSearch},
		}))
	}
}

func validateIPs(config *config_parser.HAOnPremConfigToml) *list.List {
	const notValidErrorString = "is not valid"
	errorList := list.New()

	validateIPList(config.ExistingInfra.Config.AutomatePrivateIps, "Automate private Ip", notValidErrorString, errorList)
	validateIPList(config.ExistingInfra.Config.ChefServerPrivateIps, "chef server private Ip", notValidErrorString, errorList)

	if config.ExternalDB.Database.Type != "aws" && config.ExternalDB.Database.Type != "self-managed" {
		validateIPList(config.ExistingInfra.Config.OpensearchPrivateIps, "open search private Ip", notValidErrorString, errorList)
		validateIPList(config.ExistingInfra.Config.PostgresqlPrivateIps, "Postgresql private Ip", notValidErrorString, errorList)
	}

	validateCertsByIP(config.Automate.Config.EnableCustomCerts, config.Automate.Config.CertsByIP, "Automate IP", FOR_CERTS, notValidErrorString, errorList)
	validateCertsByIP(config.ChefServer.Config.EnableCustomCerts, config.ChefServer.Config.CertsByIP, "ChefServer IP", FOR_CERTS, notValidErrorString, errorList)
	validateCertsByIP(config.Opensearch.Config.EnableCustomCerts, config.Opensearch.Config.CertsByIP, "Opensearch IP", FOR_CERTS, notValidErrorString, errorList)
	validateCertsByIP(config.Postgresql.Config.EnableCustomCerts, config.Postgresql.Config.CertsByIP, "Postgresql IP", FOR_CERTS, notValidErrorString, errorList)

	return errorList
}

func validateIPList(ipList []string, prefix string, suffix string, errorList *list.List) {
	for _, element := range ipList {
		if checkIPAddress(element) != nil {
			errorList.PushBack(prefix + " " + element + suffix)
		}
	}
}

func validateCertsByIP(enableCustomCerts bool, certsByIP []config_parser.CertByIP, prefix string, suffix string, notValidErrorString string, errorList *list.List) {
	if enableCustomCerts {
		for _, element := range certsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack(prefix + " " + element.IP + suffix + notValidErrorString)
			}
		}
	}
}
