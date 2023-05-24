package config_verify

import (
	"container/list"
	"encoding/pem"
	"errors"
	"fmt"
	"net"
	"strings"

	"github.com/chef/automate/lib/config_parser"
	"github.com/chef/automate/lib/stringutils"
)

func validateAutomateFQDN(fqdn string) error {
	if strings.Contains(fqdn, " ") {
		return errors.New("domain name cannot contain spaces")
	}

	// Check for "http://" or "https://" in the FQDN value
	if strings.HasPrefix(fqdn, "http://") || strings.HasPrefix(fqdn, "https://") {
		return fmt.Errorf("fqdn should not include protocol (http:// or https://)")
	}
	return nil
}

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
	} else {
		return nil
	}
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
		}
	}
	return errorList
}

func validateCerts(config *config_parser.HAOnPremConfigToml) *list.List {

	errorList := list.New()

	// if CustomCertsEnabled is disabled, then skip validation for custom certs and use self signed certs
	if config.Automate.Config.EnableCustomCerts {
		if len(config.Automate.Config.CertsByIP) > 0 {
			// if root_ca is provided, then check if it is valid
			if len(strings.TrimSpace(config.Automate.Config.RootCA)) > 0 {
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: config.Automate.Config.RootCA, certtype: "root_ca", svc: "automate"},
				}))
			}
			if !stringutils.SubSlice(config.ExistingInfra.Config.AutomatePrivateIps, extractIPsFromCertsByIP(config.Automate.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some automate private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(config.ExistingInfra.Config.AutomatePrivateIps, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range config.Automate.Config.CertsByIP {
				if len(strings.TrimSpace(node.IP)) < 1 ||
					len(strings.TrimSpace(node.PrivateKey)) < 1 ||
					len(strings.TrimSpace(node.PublicKey)) < 1 {
					errorList.PushBack("Field certs_by_ip for Automate requires ip, private_key and public_key. Some of them are missing.")
				}
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: node.PrivateKey, certtype: "private_key", svc: "automate cert_by_ip for ip " + node.IP},
					{key: node.PublicKey, certtype: "public_key", svc: "automate cert_by_ip for ip " + node.IP},
				}))
			}
		} else {
			// check if all the default certs are given
			if len(strings.TrimSpace(config.Automate.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(config.Automate.Config.PublicKey)) < 1 {
				errorList.PushBack("Automate public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			// if root_ca is provided, then check if it is valid
			if len(strings.TrimSpace(config.Automate.Config.RootCA)) > 0 {
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: config.Automate.Config.RootCA, certtype: "root_ca", svc: "automate"},
				}))
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config.Automate.Config.PrivateKey, certtype: "private_key", svc: "automate"},
				{key: config.Automate.Config.PublicKey, certtype: "public_key", svc: "automate"},
			}))
		}

	}

	// if CustomCertsEnabled is disabled, then skip validation for custom certs and use self signed certs
	if config.ChefServer.Config.EnableCustomCerts {
		if len(config.ChefServer.Config.CertsByIP) > 0 {
			if !stringutils.SubSlice(config.ExistingInfra.Config.ChefServerPrivateIps, extractIPsFromCertsByIP(config.ChefServer.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some ChefServer private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(config.ExistingInfra.Config.ChefServerPrivateIps, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range config.ChefServer.Config.CertsByIP {
				if len(strings.TrimSpace(node.IP)) < 1 ||
					len(strings.TrimSpace(node.PrivateKey)) < 1 ||
					len(strings.TrimSpace(node.PublicKey)) < 1 {
					errorList.PushBack("Field certs_by_ip for chef_server requires ip, private_key and public_key. Some of them are missing.")
				}
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: node.PrivateKey, certtype: "private_key", svc: "chef-server cert_by_ip for ip " + node.IP},
					{key: node.PublicKey, certtype: "public_key", svc: "chef-server cert_by_ip for ip " + node.IP},
				}))
			}
		} else {
			// check if all the default certs are given
			if len(strings.TrimSpace(config.ChefServer.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(config.ChefServer.Config.PublicKey)) < 1 {
				errorList.PushBack("ChefServer public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config.ChefServer.Config.PrivateKey, certtype: "private_key", svc: "chef-server"},
				{key: config.ChefServer.Config.PublicKey, certtype: "public_key", svc: "chef-server"},
			}))
		}
	}

	// if CustomCertsEnabled is disabled, then skip validation for custom certs and use self signed certs
	if config.Postgresql.Config.EnableCustomCerts {
		config_onprem_postgres := config.Postgresql
		if len(config_onprem_postgres.Config.CertsByIP) > 0 {
			if len(strings.TrimSpace(config_onprem_postgres.Config.RootCA)) < 1 {
				errorList.PushBack("Postgresql root_ca is missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config_onprem_postgres.Config.RootCA, certtype: "root_ca", svc: "postgresql"},
			}))
			if !stringutils.SubSlice(config.ExistingInfra.Config.PostgresqlPrivateIps, extractIPsFromCertsByIP(config_onprem_postgres.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some Postgresql private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(config.ExistingInfra.Config.PostgresqlPrivateIps, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range config_onprem_postgres.Config.CertsByIP {
				if len(strings.TrimSpace(node.IP)) < 1 ||
					len(strings.TrimSpace(node.PrivateKey)) < 1 ||
					len(strings.TrimSpace(node.PublicKey)) < 1 {
					errorList.PushBack("Field certs_by_ip for postgresql requires ip, private_key and public_key. Some of them are missing.")
				}
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: node.PrivateKey, certtype: "private_key", svc: "postgresql cert_by_ip for ip " + node.IP},
					{key: node.PublicKey, certtype: "public_key", svc: "postgresql cert_by_ip for ip " + node.IP},
				}))
			}
		} else {
			// check if all the default certs are given

			if len(strings.TrimSpace(config_onprem_postgres.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(config_onprem_postgres.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(config_onprem_postgres.Config.PublicKey)) < 1 {
				errorList.PushBack("Postgresql root_ca and/or public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config_onprem_postgres.Config.RootCA, certtype: "root_ca", svc: "postgresql"},
				{key: config_onprem_postgres.Config.PrivateKey, certtype: "private_key", svc: "postgresql"},
				{key: config_onprem_postgres.Config.PublicKey, certtype: "public_key", svc: "postgresql"},
			}))
		}
	}

	// if CustomCertsEnabled is disabled, then skip validation for custom certs and use self signed certs
	if config.Opensearch.Config.EnableCustomCerts {
		config_onprem_opensearch := config.Opensearch
		if len(config_onprem_opensearch.Config.CertsByIP) > 0 {
			if len(strings.TrimSpace(config_onprem_opensearch.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(config_onprem_opensearch.Config.AdminKey)) < 1 ||
				len(strings.TrimSpace(config_onprem_opensearch.Config.AdminCert)) < 1 {
				errorList.PushBack("Opensearch root_ca, admin_key or admin_cert is missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config_onprem_opensearch.Config.RootCA, certtype: "root_ca", svc: "opensearch"},
				{key: config_onprem_opensearch.Config.AdminKey, certtype: "admin_key", svc: "opensearch"},
				{key: config_onprem_opensearch.Config.AdminCert, certtype: "admin_cert", svc: "opensearch"},
			}))
			if !stringutils.SubSlice(config.ExistingInfra.Config.OpensearchPrivateIps, extractIPsFromCertsByIP(config_onprem_opensearch.Config.CertsByIP)) {
				errorList.PushBack("Missing certificates for some Opensearch private ips. Please make sure certificates for the following ips are provided in certs_by_ip: " + strings.Join(config.ExistingInfra.Config.OpensearchPrivateIps, ", "))
			}
			// check if all the certs are valid for given IPs
			for _, node := range config.Opensearch.Config.CertsByIP {
				if len(strings.TrimSpace(node.IP)) < 1 ||
					len(strings.TrimSpace(node.PrivateKey)) < 1 ||
					len(strings.TrimSpace(node.PublicKey)) < 1 {
					errorList.PushBack("Field certs_by_ip for opensearch requires ip, private_key and public_key. Some of them are missing.")
				}
				errorList.PushBackList(checkCertValid([]keydetails{
					{key: node.PrivateKey, certtype: "private_key", svc: "opensearch cert_by_ip for ip " + node.IP},
					{key: node.PublicKey, certtype: "public_key", svc: "opensearch cert_by_ip for ip " + node.IP},
				}))
			}
		} else {
			// check if all the default certs are given
			if len(strings.TrimSpace(config_onprem_opensearch.Config.RootCA)) < 1 ||
				len(strings.TrimSpace(config_onprem_opensearch.Config.AdminKey)) < 1 ||
				len(strings.TrimSpace(config_onprem_opensearch.Config.AdminCert)) < 1 ||
				len(strings.TrimSpace(config_onprem_opensearch.Config.PrivateKey)) < 1 ||
				len(strings.TrimSpace(config_onprem_opensearch.Config.PublicKey)) < 1 {
				errorList.PushBack("Opensearch root_ca and/or admin_key and/or admin_cert and/or public_key and/or private_key are missing. Set custom_certs_enabled to false to continue without custom certificates.")
			}
			errorList.PushBackList(checkCertValid([]keydetails{
				{key: config_onprem_opensearch.Config.RootCA, certtype: "root_ca", svc: "opensearch"},
				{key: config_onprem_opensearch.Config.AdminKey, certtype: "admin_key", svc: "opensearch"},
				{key: config_onprem_opensearch.Config.AdminCert, certtype: "admin_cert", svc: "opensearch"},
				{key: config_onprem_opensearch.Config.PrivateKey, certtype: "private_key", svc: "opensearch"},
				{key: config_onprem_opensearch.Config.PublicKey, certtype: "public_key", svc: "opensearch"},
			}))
		}
	}
	return errorList
}

func validateIPs(config *config_parser.HAOnPremConfigToml) *list.List {
	const notValidErrorString = "is not valid"
	errorList := list.New()

	for _, element := range config.ExistingInfra.Config.AutomatePrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("Automate private Ip " + element + notValidErrorString)
		}
	}

	for _, element := range config.ExistingInfra.Config.ChefServerPrivateIps {
		if checkIPAddress(element) != nil {
			errorList.PushBack("chef server private Ip " + element + notValidErrorString)
		}
	}

	if (config.ExternalDB.Database.Type != "aws") && (config.ExternalDB.Database.Type != "self-managed") {
		for _, element := range config.ExistingInfra.Config.OpensearchPrivateIps {
			if checkIPAddress(element) != nil {
				errorList.PushBack("open search private Ip " + element + notValidErrorString)
			}
		}

		for _, element := range config.ExistingInfra.Config.PostgresqlPrivateIps {
			if checkIPAddress(element) != nil {
				errorList.PushBack("Postgresql private Ip " + element + notValidErrorString)
			}
		}
	}

	if config.Automate.Config.EnableCustomCerts {
		for _, element := range config.Automate.Config.CertsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack("Automate IP " + element.IP + " for certs " + notValidErrorString)
			}
		}
	}
	if config.ChefServer.Config.EnableCustomCerts {
		for _, element := range config.ChefServer.Config.CertsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack("ChefServer IP " + element.IP + " for certs " + notValidErrorString)
			}
		}
	}

	if config.Opensearch.Config.EnableCustomCerts {
		for _, element := range config.Opensearch.Config.CertsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack("Opensearch IP " + element.IP + " for certs " + notValidErrorString)
			}
		}
	}

	if config.Postgresql.Config.EnableCustomCerts {
		for _, element := range config.Postgresql.Config.CertsByIP {
			if checkIPAddress(element.IP) != nil {
				errorList.PushBack("Postgresql IP " + element.IP + " for certs " + notValidErrorString)
			}
		}
	}

	return errorList
}
