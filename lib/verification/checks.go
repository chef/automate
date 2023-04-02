package verification

import (
	sc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/lib/config_parser"
	"github.com/chef/automate/lib/reporting"
)

const (
	systemResources  = "System Resources"
	softwareVersions = "Software Versions"
)

type Result struct {
	Valid  bool
	Report reporting.Info
}

func validateAWSProvisionConfig(config *config_parser.HAAwsConfigToml) {

	/* Write the checks to validate if all the required fields for AWS Infra Provision without Managed Services
	are filled in the parsed config */

	if config.Aws.Config.SetupManagedServices {
		/* Write the checks to validate if all the required fields for AWS Infra Provision with Managed Services
		are filled in the parsed config */
	}

}

func validateAWSManagedProvisionConfig(config *config_parser.HAAwsConfigToml) {

	/* Write the checks to validate if all the required fields for AWS Infra Provision with Managed Services
	are filled in the parsed config */

}

func validateAWSDeploymentConfig(config *config_parser.HAAwsConfigToml) {

	/* Write the checks to validate if all the required fields for AWS Deployment without Managed Services
	are filled in the parsed config */

}

func validateAWSManagedDeploymentConfig(config *config_parser.HAAwsConfigToml) {

	/* Write the checks to validate if all the required fields for AWS Deployment with Managed Services
	are filled in the parsed config */

}

func validateOnPremConfig(config *config_parser.HAOnPremConfigToml) {

	/* Write the checks to validate if all the required fields for On-Prem Deployment with Chef Managed Services
	are filled in the parsed config */

}

func validateOnPremAWSConfig(config *config_parser.HAOnPremConfigToml) {

	/* Write the checks to validate if all the required fields for On-Prem Deployment with AWS Managed Services
	are filled in the parsed config */

}

func validateOnPremCustomerConfig(config *config_parser.HAOnPremConfigToml) {

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
		SummaryInfo: &reporting.SummaryInfo{
			SuccessfulCount: 1,
		},
	}

	chanWriter(channel, nodeType, result)

}

func validateCertificateExpiry(ipaddress string, certContents string) Result {

	return Result{
		Valid: true,
		Report: reporting.Info{
			Hostip:    ipaddress,
			Parameter: "Certificates",
			Status:    "Success",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "Certificate A validated successfully",
			},
		},
	}

	/* Write the checks to validate the expiry date of the certificate */

}

func validateCertificateFormat(ipaddress string, certContents string) Result {

	return Result{
		Valid: true,
		Report: reporting.Info{
			Hostip:    ipaddress,
			Parameter: "Certificates",
			Status:    "Failed",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "Certificate B validation failed",
				SubMessage:  []string{"Certificate is not formatted properly"},
			},
			SummaryInfo: &reporting.SummaryInfo{
				ToResolve: []string{"Add the start and end certificate markers"},
			},
		},
	}

	/* Write the checks to validate the expiry date of the certificate */

}

func validateFreeDisk(ipaddress string, nodeType string) Result {

	if nodeType == "OpenSearch" {
		return Result{
			Valid: false,
			Report: reporting.Info{
				Hostip:    ipaddress,
				Parameter: systemResources,
				Status:    "Failed",
				StatusMessage: &reporting.StatusMessage{
					MainMessage: "System Resources check failed",
					SubMessage:  []string{"/hab volume is not of 100 GB"},
				},
				SummaryInfo: &reporting.SummaryInfo{
					ToResolve: []string{"Re-mount the hab volume with 100 GB"},
				},
			},
		}
	} else {
		return Result{
			Valid: true,
			Report: reporting.Info{
				Hostip:    ipaddress,
				Parameter: systemResources,
				Status:    "Success",
				StatusMessage: &reporting.StatusMessage{
					MainMessage: "System Resources as per the requirement",
				},
			},
		}
	}

}

func validateCPU(ipaddress string, nodeType string) Result {

	if nodeType == "ChefServer" {
		return Result{
			Valid: true,
			Report: reporting.Info{
				Hostip:    ipaddress,
				Parameter: systemResources,
				Status:    "Success",
				StatusMessage: &reporting.StatusMessage{
					MainMessage: "System Resources as per the requirement",
				},
			},
		}
	} else {
		return Result{
			Valid: false,
			Report: reporting.Info{
				Hostip:    ipaddress,
				Parameter: systemResources,
				Status:    "Failed",
				StatusMessage: &reporting.StatusMessage{
					MainMessage: "System Resources check failed",
					SubMessage:  []string{"CPU count is not as per the requirement"},
				},
				SummaryInfo: &reporting.SummaryInfo{
					ToResolve: []string{"Increase the CPU of the machine. Machine should have 4 CPUs"},
				},
			},
		}
	}

}

func validateOSVersion(ipaddress string, nodeType string) Result {

	if nodeType == "Automate" {
		return Result{
			Valid: false,
			Report: reporting.Info{
				Hostip:    ipaddress,
				Parameter: softwareVersions,
				Status:    "Failed",
				StatusMessage: &reporting.StatusMessage{
					MainMessage: "Software Versions check Failed",
					SubMessage:  []string{"OS version is not Ubuntu 20.04"},
				},
				SummaryInfo: &reporting.SummaryInfo{
					ToResolve: []string{"Setup the machine with Ubuntu 20.04"},
				},
			},
		}
	} else {
		return Result{
			Valid: true,
			Report: reporting.Info{
				Hostip:    ipaddress,
				Parameter: softwareVersions,
				Status:    "Success",
				StatusMessage: &reporting.StatusMessage{
					MainMessage: "Software Versions check Passed",
				},
			},
		}
	}
}

func validateLinuxCommands(ipaddress string, nodeType string) Result {

	if nodeType == "OpenSearch" {
		return Result{
			Valid: true,
			Report: reporting.Info{
				Hostip:    ipaddress,
				Parameter: softwareVersions,
				Status:    "Success",
				StatusMessage: &reporting.StatusMessage{
					MainMessage: "Software Versions check Failed",
					SubMessage:  []string{"make command failed"},
				},
				SummaryInfo: &reporting.SummaryInfo{
					ToResolve: []string{"Add the required paths in the PATH folder"},
				},
			},
		}
	}

	return Result{
		Valid: true,
		Report: reporting.Info{
			Hostip:    ipaddress,
			Parameter: softwareVersions,
			Status:    "Success",
			StatusMessage: &reporting.StatusMessage{
				MainMessage: "Software Versions check Passed",
			},
		},
	}

}
