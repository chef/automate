package main

import (
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/toml"
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
			return strings.ToUpper(AUTOMATE), nil
		}
	} else if checkIfFileExist(filepath.Join(initConfigHabA2HAPathFlag.a2haDirPath, "a2ha.rb")) {
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

func setDefaultCertsForBackend(osConfig *OsConfigToml, pgConfig *PgConfigToml) error {
	var defaultConfig DefaultBackendCerts
	if !osConfig.EnableCustomCerts || !pgConfig.EnableCustomCerts {
		// reading toml file at "/hab/default_backend_certificates.toml" and read the root_ca, ssl_cert and ssl key from it
		// and set it in the config
		defaultToml, err := os.ReadFile(DEFAULT_BACKEND_CERTS)
		if err != nil {
			return err
		}
		err = toml.Unmarshal(defaultToml, &defaultConfig)
		if err != nil {
			return err
		}
	}
	if !osConfig.EnableCustomCerts {
		osConfig.EnableCustomCerts = true

		// set the root_ca, ssl_cert and ssl_key in the config
		osConfig.RootCA = fmt.Sprintf("%v", defaultConfig.RootCA)
		osConfig.AdminCert = fmt.Sprintf("%v", defaultConfig.AdminCert)
		osConfig.AdminKey = fmt.Sprintf("%v", defaultConfig.AdminKey)
		osConfig.PublicKey = fmt.Sprintf("%v", defaultConfig.SslCert)
		osConfig.PrivateKey = fmt.Sprintf("%v", defaultConfig.SslKey)
	}

	if !pgConfig.EnableCustomCerts {
		pgConfig.EnableCustomCerts = true
		pgConfig.RootCA = fmt.Sprintf("%v", defaultConfig.RootCA)
		pgConfig.PublicKey = fmt.Sprintf("%v", defaultConfig.SslCert)
		pgConfig.PrivateKey = fmt.Sprintf("%v", defaultConfig.SslKey)
	}
	return nil
}
