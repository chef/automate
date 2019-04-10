package certs

import (
	"crypto/tls"
	"crypto/x509"
	"encoding/pem"
	"io/ioutil"
	"path"

	"github.com/pkg/errors"
)

// TLSConfig is the on-disk paths to the service cert, service key, and root CA
// cert. It represents the TLS config that every service needs to supply a
// configuration interface for, in order to use mutual TLS
//
// Tags are provided for JSON and TOML, which allows configuration to be
// automatically unmarshaled from these formats plus YAML when using
// https://github.com/ghodss/yaml
//
// Example (TOML)
// 	type YourConfig struct {
// 		MyOpt	string `toml:"toml_key"`
// 		TLSConfig	`toml:"tls"`
// 	}
//
// Example (YAML via /ghodss/yaml)
// 	type YourConfig struct {
//   	MyOpt	string `json:"json_key"`
// 		TLSConfig	`json:"tls"`
// 	}
type TLSConfig struct {
	CertPath       string `json:"cert_path" toml:"cert_path" mapstructure:"cert_path"`
	KeyPath        string `json:"key_path" toml:"key_path" mapstructure:"key_path"`
	RootCACertPath string `json:"root_ca_path" toml:"root_ca_path" mapstructure:"root_ca_path"`
}

// ServiceCerts contains the key pair the service should use, along with the CA cert to use
// for verifying clients
type ServiceCerts struct {
	ServiceKeyPair *tls.Certificate
	RootCACert     *x509.Certificate
}

// In config files, paths to TLSConfig members are relative to the file, not
// CWD; we need to adjust the paths before we can pass them to a call like
// file.Open.
func (config *TLSConfig) FixupRelativeTLSPaths(configFilePath string) {
	config.CertPath = fixPath(configFilePath, config.CertPath)
	config.KeyPath = fixPath(configFilePath, config.KeyPath)
	config.RootCACertPath = fixPath(configFilePath, config.RootCACertPath)
}

// If filePath is relative, transform it into being relative to the config file
func fixPath(configPath string, filePath string) string {
	if filePath == "" || path.IsAbs(filePath) {
		return filePath
	}

	return path.Join(path.Dir(configPath), filePath)
}

func (config *TLSConfig) ReadCerts() (*ServiceCerts, error) {
	if config.CertPath == "" {
		return nil, errors.New("Path to certificate not given")
	}

	if config.KeyPath == "" {
		return nil, errors.New("Path to certificate key not given")
	}

	if config.RootCACertPath == "" {
		return nil, errors.New("Path to Root CA certificate not given")
	}

	serviceCertData, err := ioutil.ReadFile(config.CertPath)
	if err != nil {
		return nil, errors.Wrap(err, "Could not read the service cert")
	}

	serviceKeyData, err := ioutil.ReadFile(config.KeyPath)
	if err != nil {
		return nil, errors.Wrap(err, "Could not read the service key")
	}

	rootCertData, err := ioutil.ReadFile(config.RootCACertPath)
	if err != nil {
		return nil, errors.Wrap(err, "Could not read the root ca cert")
	}

	return ServiceCertsFromBytes(serviceCertData, serviceKeyData, rootCertData)
}

func ServiceCertsFromBytes(serviceCertData, serviceKeyData, rootCertData []byte) (*ServiceCerts, error) {
	serviceKeyPair, err := tls.X509KeyPair(serviceCertData, serviceKeyData)
	if err != nil {
		return nil, errors.Wrap(err, "Could not load key pair")
	}

	block, _ := pem.Decode(rootCertData)
	if block == nil {
		return nil, errors.New("Failed to parse certificate PEM")
	}

	rootCert, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to parse certificate")
	}

	return &ServiceCerts{&serviceKeyPair, rootCert}, nil
}

// NewCertPool returns a x509.CertPool configured with the ServiceCerts' root ca
func (serviceCerts *ServiceCerts) NewCertPool() *x509.CertPool {
	certPool := x509.NewCertPool()
	certPool.AddCert(serviceCerts.RootCACert)
	return certPool
}
