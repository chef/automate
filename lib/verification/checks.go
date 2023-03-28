package verification

import (
	sc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/lib/reporting"
)

func validateAWSProvisionConfig(config *HAAwsConfigToml) {

	/* Write the checks to validate if all the required fields for AWS Infra Provision without Managed Services
	are filled in the parsed config */

	if config.Aws.Config.SetupManagedServices {

	}

}

func validateAWSManagedProvisionConfig(config *HAAwsConfigToml) {

	/* Write the checks to validate if all the required fields for AWS Infra Provision with Managed Services
	are filled in the parsed config */

}

func validateAWSDeploymentConfig(config *HAAwsConfigToml) {

	/* Write the checks to validate if all the required fields for AWS Deployment without Managed Services
	are filled in the parsed config */

}

func validateAWSManagedDeploymentConfig(config *HAAwsConfigToml) {

	/* Write the checks to validate if all the required fields for AWS Deployment with Managed Services
	are filled in the parsed config */

}

func validateOnPremConfig(config *HAOnPremConfigToml) {

	/* Write the checks to validate if all the required fields for On-Prem Deployment with Chef Managed Services
	are filled in the parsed config */

}

func validateOnPremAWSConfig(config *HAOnPremConfigToml) {

	/* Write the checks to validate if all the required fields for On-Prem Deployment with AWS Managed Services
	are filled in the parsed config */

}

func validateOnPremCustomerConfig(config *HAOnPremConfigToml) {

	/* Write the checks to validate if all the required fields for On-Prem Deployment with Customer Managed Services
	are filled in the parsed config */

}

func validateStandaloneDeploymentConfig(config *sc.AutomateConfig) {

	/* Write the checks to validate if all the required fields for On-Prem Deployment with Customer Managed Services
	are filled in the parsed config */

}

func validateNodeReachability(username string, keyfile string, ipaddress string) {

	/* Write the checks to validate if the nodes are reachable */

}

func validateCertificateExpiry(certContents string) reporting.Info {
	return reporting.Info{
		Hostip:    "172.02.0.01",
		Parameter: "Certificate",
		Status:    "Success",
		StatusMessage: &reporting.StatusMessage{
			MainMessage: "Certificate A validated successfully",
		},
	}
	/* Write the checks to validate the expiry date of the certificate */

}
