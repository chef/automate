package verification

import (
	sc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/lib/reporting"
)

type Result struct {
	Valid  bool
	Report reporting.Info
}

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

func validateNodeReachability(ipaddress string, nodeType string, channel chan reporting.VerfictionReport) {

	/* Write the checks to validate if the nodes are reachable */

	result := reporting.Info{
		Hostip:    ipaddress,
		Parameter: "Node Reachabilility",
		Status:    "Success",
		StatusMessage: &reporting.StatusMessage{
			MainMessage: "Node " + ipaddress + " is reachable",
		},
	}

	chanWriter(channel, nodeType, result)

}

func validateCertificateExpiry(certContents string) Result {

	return Result{
		Valid: true,
		Report: reporting.Info{
			Hostip:    "172.02.0.01",
			Parameter: "Certificates",
			Status:    "Success",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "Certificate A validated successfully",
			},
		},
	}

	// return Result{
	// 	Valid: false,
	// 	Report: reporting.Info{
	// 		Hostip:    "172.02.0.01",
	// 		Parameter: "Certificates",
	// 		Status:    "Failed",
	// 		StatusMessage: &reporting.StatusMessage{
	// 			MainMessage: "Certificate B validation failed",
	// 			SubMessage:  []string{"Certificate is not valid"},
	// 			ToResolve:   []string{"1. Renew the expired certificates"},
	// 		},
	// 	},
	// }

	/* Write the checks to validate the expiry date of the certificate */

}

func validateCertificateFormat(certContents string) Result {

	return Result{
		Valid: true,
		Report: reporting.Info{
			Hostip:    "172.02.20.01",
			Parameter: "Certificates",
			Status:    "Failed",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "Certificate B validation failed",
				SubMessage:  []string{"Certificate is not formatted properly"},
				ToResolve:   []string{"1. Add the start and end certificate markers"},
			},
		},
	}

	/* Write the checks to validate the expiry date of the certificate */

}

func validateFreeDisk(ipaddress string) Result {

	return Result{
		Valid: false,
		Report: reporting.Info{
			Hostip:    ipaddress,
			Parameter: "System Resources",
			Status:    "Failed",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "Disk space is not as per the requirements",
				SubMessage:  []string{"/hab is not of 100 GB"},
				ToResolve:   []string{"1. Re-mount the hab volume with 100 GB"},
			},
		},
	}

}

func validateCPU(ipaddress string) Result {

	return Result{
		Valid: false,
		Report: reporting.Info{
			Hostip:    ipaddress,
			Parameter: "System Resources",
			Status:    "Failed",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "CPU is not as per the requirements",
				SubMessage:  []string{"Machine has lesser number of CPUs"},
				ToResolve:   []string{"1. Increase the CPU of the machine"},
			},
		},
	}

}

func validateOSVersion(ipaddress string) Result {

	return Result{
		Valid: false,
		Report: reporting.Info{
			Hostip:    ipaddress,
			Parameter: "Software Versions",
			Status:    "Failed",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "Software Versions check Failed",
				SubMessage:  []string{"OS version is not Ubuntu 20.04"},
				ToResolve:   []string{"1. Setup the machine with Ubuntu 20.04"},
			},
		},
	}

}

func validateLinuxCommands(ipaddress string) Result {

	return Result{
		Valid: true,
		Report: reporting.Info{
			Hostip:    ipaddress,
			Parameter: "Software Versions",
			Status:    "Success",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "Software Versions check Passed",
			},
		},
	}

}
