package config_parser

import (
	"path/filepath"

	sc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/io/fileutils"
	ptoml "github.com/pelletier/go-toml"
)

type ConfigParser interface {
	ParseAWSAutomateConfig(configFile string) (*HAAwsConfigToml, error)
	ParseOnPremConfig(configFile string) (*HAOnPremConfigToml, error)
	ParseStandaloneConfig(configFile string) (*sc.AutomateConfig, error)
	ParseDeploymentType(configFile string) (string, error)
}

type ConfigParserImpl struct{}

func ParseDeploymentType(configPath string) (string, error) {
	initConfigHAPath := initConfigHAPathFlags.path
	if len(configPath) > 0 {
		initConfigHAPath = configPath
	}
	if checkIfFileExist(initConfigHAPath) {
		config, err := ptoml.LoadFile(initConfigHAPath)
		if err != nil {
			writer.Println(err.Error())
			return "", err
		}
		if config.Get("architecture.existing_infra") != nil {
			return EXISTING_INFRA_MODE, nil
		} else if config.Get("architecture.aws") != nil {
			return AWS_MODE, nil
		} else {
			return AUTOMATE, nil
		}
	} else if checkIfFileExist(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "a2ha.rb")) {
		return HA_MODE, nil
	} else {
		return AUTOMATE, nil
	}
}

func getDeployer(configPath string) (deployManager, error) {
	deployerType, err := getModeFromConfig(configPath)
	if err != nil {
		return nil, err
	}
	if deployerType == AWS_MODE {
		return newAwsDeployemnt(configPath), nil
	}
	if deployerType == EXISTING_INFRA_MODE {
		return newExistingInfa(configPath), nil
	}
	if deployerType == HA_MODE {
		return newHaWithoutConfig(), nil
	}
	return nil, nil
}

func (cp *ConfigParserImpl) ParseAWSAutomateConfig(configFile string) (*HAAwsConfigToml, error) {
	return ParseAWSAutomateConfig(configFile)
}

func (cp *ConfigParserImpl) ParseOnPremConfig(configFile string) (*HAOnPremConfigToml, error) {
	return ParseOnPremConfig(configFile)
}

func (cp *ConfigParserImpl) ParseStandaloneConfig(configFile string) (*sc.AutomateConfig, error) {
	return ParseStandaloneConfig(configFile)
}

func ParseAWSAutomateConfig(configFile string) (*HAAwsConfigToml, error) {

	/* This function will read the config toml file and will try to parse it in the structure.
	   On successful parse, it will return the config structure. This is applicable for both
	   AWS Provision and Deployment configuration both with Chef Managed and AWS Managed resources. */

	fileUtils := &fileutils.FileSystemUtils{}
	templateBytes, err := fileUtils.ReadFile(configFile)
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := HAAwsConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return &config, nil
}

func ParseOnPremConfig(configFile string) (*HAOnPremConfigToml, error) {

	/* This function will read the OnPrem config toml file and will try to parse it in the structure.
	   On successful parse, it will return the config structure. This is applicable for Chef Managed,
	   AWS Managed and Customer Managed resources*/

	fileUtils := &fileutils.FileSystemUtils{}
	templateBytes, err := fileUtils.ReadFile(configFile)
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := HAOnPremConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return &config, nil
}

func ParseStandaloneConfig(configFile string) (*sc.AutomateConfig, error) {

	/* This function will read the Standalone Automate config toml file and will try to parse it in the structure.
	   On successful parse, it will return the config structure. */

	config, err := sc.LoadUserOverrideConfigFile(configFile)
	if err != nil {
		return nil, err
	}
	return config, nil
}
