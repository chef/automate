package config_verify

import (
	sc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/lib/config_parser"
)

type ConfigVerify interface {
	ConfigValidateAWS(config *config_parser.HAAwsConfigToml)
	ConfigValidateOnPrem(config *config_parser.HAOnPremConfigToml)
	ParseAWSAutomateConfig(configFile string) (*HAAwsConfigToml, error)
	ParseOnPremConfig(configFile string) (*HAOnPremConfigToml, error)
	ParseStandaloneConfig(configFile string) (*sc.AutomateConfig, error)
	ParseDeploymentType
}

type ConfigVerifyImpl struct{}
