package main

import (
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"io/ioutil"
	"net"
	"path/filepath"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
)

func getModeFromConfig(configPath string) (string, error) {
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

func getDeploymentModeFromConfig(configPath string) (string, error) {
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
			if config.Get("external.database") != nil {
				if config.Get("external.database.type") == "aws" {
					return EXISTING_INFRA_AWS_MANAGED, nil
				} else if config.Get("external.database.type") == "self-managed" {
					return EXISTING_INFRA_SELF_MANAGED, nil
				}
			}
			return EXISTING_INFRA_MODE, nil
		} else if config.Get("architecture.aws") != nil {
			if config.Get("aws.config") != nil {
				value := config.Get("aws.config.setup_managed_services")
				if boolValue, ok := value.(bool); ok {
					if boolValue {
						return AWS_MANAGED_SERVICES, nil
					}
				} else {
					return "", status.New(status.ConfigError, "aws.config.setup_managed_services value has to be boolean")
				}
			}
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

func checkIPAddress(ip string) error {
	if net.ParseIP(ip) == nil {
		return errors.New("Ip Address is invalid.")
	} else {
		return nil
	}
}

func getExistingInfraConfig(configPath string) (*ExistingInfraConfigToml, error) {
	templateBytes, err := ioutil.ReadFile(configPath) // nosemgrep
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := ExistingInfraConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return &config, nil
}

func getAwsConfig(configPath string) (*AwsConfigToml, error) {
	templateBytes, err := ioutil.ReadFile(configPath) // nosemgrep
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := AwsConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return &config, nil
}

func checkSharedConfigFile() bool {
	if checkIfFileExist(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "config.toml")) {
		return true
	}
	return false
}

func getDistinguishedNameFromKey(publicKey string) (pkix.Name, error) {
	block, _ := pem.Decode([]byte(publicKey))
	if block == nil {
		return pkix.Name{}, status.New(status.ConfigError, "failed to decode certificate PEM")
	}
	cert, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		return pkix.Name{}, status.Wrap(err, status.ConfigError, "failed to parse certificate PEM")
	}
	return cert.Subject, nil
}
