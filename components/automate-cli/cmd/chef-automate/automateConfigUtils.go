package main

import (
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"io/ioutil"
	"net"
	"path/filepath"
	"strings"

	pgc "github.com/chef/automate/components/automate-cli/pkg/pullandgenerateconfig"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	ptoml "github.com/pelletier/go-toml"
	"github.com/pkg/errors"
)

func getModeFromConfig(configPath string) (string, error) {
	initConfigHAPath := pgc.InitConfigHAPathFlags.Path
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
			return strings.ToUpper(AUTOMATE), nil
		}
	} else if checkIfFileExist(filepath.Join(pgc.InitConfigHabA2HAPathFlag.A2haDirPath, "a2ha.rb")) {
		return HA_MODE, nil
	} else {
		return strings.ToUpper(AUTOMATE), nil
	}
}

func checkIPAddress(ip string) error {
	if net.ParseIP(ip) == nil {
		return errors.New("Ip Address is invalid.")
	} else {
		return nil
	}
}

func getExistingInfraConfig(configPath string) (*pgc.ExistingInfraConfigToml, error) {
	templateBytes, err := ioutil.ReadFile(configPath) // nosemgrep
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := pgc.ExistingInfraConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return &config, nil
}

func getAwsConfig(configPath string) (*pgc.AwsConfigToml, error) {
	templateBytes, err := ioutil.ReadFile(configPath) // nosemgrep
	if err != nil {
		return nil, status.Wrap(err, status.FileAccessError, "error in reading config toml file")
	}
	config := pgc.AwsConfigToml{}
	err = ptoml.Unmarshal(templateBytes, &config)
	if err != nil {
		return nil, status.Wrap(err, status.ConfigError, "error in unmarshalling config toml file")
	}
	return &config, nil
}

func checkSharedConfigFile() bool {
	if checkIfFileExist(filepath.Join(pgc.InitConfigHabA2HAPathFlag.A2haDirPath, "config.toml")) {
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
