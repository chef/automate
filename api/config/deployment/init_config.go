package deployment

import (
	"bytes"
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/hex"
	"encoding/pem"
	"fmt"
	"io/ioutil"
	"math/big"
	"net"
	"net/url"
	"os"
	"strconv"
	"strings"
	"text/template"
	"time"

	"github.com/pkg/errors"

	es "github.com/chef/automate/api/config/elasticsearch"
	license_control "github.com/chef/automate/api/config/license_control"
	global "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/chef/automate/lib/proxy"
	"github.com/chef/automate/lib/stringutils"
)

// InitConfig is a struct that contains fields that correspond to user facing
// configuration values that we'll use when rendering the config template.
type InitConfig struct {
	DeploymentType  string
	Channel         string
	UpgradeStrategy string
	Fqdn            string
	FrontendCert    string
	FrontendKey     string
	License         string
	TLSWarning      string
	ProxyHost       string
	ProxyPort       int32
	ProxyUser       string
	ProxyPassword   string
	NoProxy         []string
	ESHeapSize      string
}

// NewInitConfig returns a new instance of InitConfig with default values
func NewInitConfig() *InitConfig {
	return &InitConfig{}
}

// InitConfigOpt is an option that can be passed to the
// GenerateInitConfig
type InitConfigOpt func(*InitConfig) error

// InitialFQDN sets the FQDN for the generated configuration
func InitialFQDN(fqdn string) InitConfigOpt {
	return func(c *InitConfig) error {
		c.Fqdn = fqdn
		return nil
	}
}

// ESMem sets the ES heap size for the generated configuration
func ESMem(mem string) InitConfigOpt {
	return func(c *InitConfig) error {
		c.ESHeapSize = mem
		return nil
	}
}

// InitialTLSCerts sets the TLS certificates in the generated
// configuration to those contained in the files at the specified path
func InitialTLSCerts(keyPath string, certPath string) InitConfigOpt {
	return func(c *InitConfig) error {
		if keyPath != "" && certPath != "" {
			return tlsFromUser(c, keyPath, certPath)
		}

		return nil
	}
}

// GenerateInitConfig constructions an InitConfig, generating values
// not passed by the caller if possible.
func GenerateInitConfig(channel string, upgradeStrategy string, opts ...InitConfigOpt) (*InitConfig, error) {
	var err error

	cfg := NewInitConfig()
	for _, o := range opts {
		err = o(cfg)
		if err != nil {
			return nil, err
		}
	}

	// Non-optional
	cfg.UpgradeStrategy = upgradeStrategy

	// Non-configurable
	cfg.DeploymentType = "local"

	// Autogenerate configurables that we don't have
	if channel == "" {
		cfg.Channel = "current"
	} else {
		cfg.Channel = channel
	}

	if cfg.Fqdn == "" {
		cfg.Fqdn = LbFQDN()
	}

	if cfg.FrontendCert == "" && cfg.FrontendKey == "" {
		err = generateTLS(cfg)
		if err != nil {
			return nil, err
		}
	}

	err = generateProxySettings(cfg)
	if err != nil {
		return nil, err
	}

	if cfg.ESHeapSize == "" {
		cfg.ESHeapSize = esHeapSize()
	}

	return cfg, nil
}

// Render returns a user facing subset of the AutomateConfig as a TOML string.
func (c InitConfig) Render() (string, error) {
	temp := template.Must(template.New("init").
		Funcs(template.FuncMap{"StringsJoin": strings.Join}).
		Parse(configTemplate))

	var buf bytes.Buffer
	err := temp.Execute(&buf, c)
	if err != nil {
		return "", err
	}

	return buf.String(), nil
}

// AutomateConfig returns an AutomateConfig with the appropriate
// fields from an InitConfig filled in
func (c InitConfig) AutomateConfig() *AutomateConfig {
	// Fill out global config
	g := global.DefaultGlobalConfig()
	g.V1.Fqdn = w.String(c.Fqdn)
	g.V1.FrontendTls = []*global.FrontendTLSCredential{
		{
			Cert: c.FrontendCert,
			Key:  c.FrontendKey,
		},
	}

	if c.ProxyHost != "" {
		g.V1.Proxy = &global.Proxy{}
		g.V1.Proxy.Host = w.String(c.ProxyHost)
		if c.ProxyPort != 0 {
			g.V1.Proxy.Port = w.Int32(c.ProxyPort)
		}
		if c.ProxyUser != "" {
			g.V1.Proxy.User = w.String(c.ProxyUser)
		}
		if c.ProxyPassword != "" {
			g.V1.Proxy.Password = w.String(c.ProxyPassword)
		}
		if len(c.NoProxy) > 0 {
			g.V1.Proxy.NoProxy = c.NoProxy
		}
	}

	// Fill out automate config
	cfg := AutomateConfig{}
	cfg.Global = g

	cfg.SetChannel(c.Channel)                 // nolint: errcheck
	cfg.SetUpgradeStrategy(c.UpgradeStrategy) // nolint: errcheck

	cfg.OverrideConfigValues(&AutomateConfig{ // nolint: errcheck
		Deployment: &ConfigRequest{
			V1: &ConfigRequest_V1{
				Svc: &ConfigRequest_V1_Service{
					DeploymentType: w.String(c.DeploymentType),
				},
			},
		},
	})

	cfg.OverrideConfigValues(&AutomateConfig{ // nolint: errcheck
		LicenseControl: &license_control.ConfigRequest{
			V1: &license_control.ConfigRequest_V1{
				Svc: &license_control.ConfigRequest_V1_Service{
					License: w.String(c.License),
				},
			},
		},
	})

	cfg.OverrideConfigValues(&AutomateConfig{ // nolint: errcheck
		Elasticsearch: &es.ConfigRequest{
			V1: &es.ConfigRequest_V1{
				Sys: &es.ConfigRequest_V1_System{
					Runtime: &es.ConfigRequest_V1_Runtime{
						Heapsize: w.String(c.ESHeapSize),
					},
				},
			},
		},
	})

	return &cfg
}

// LbFQDN -- First try executing `hostname -f` and if that fails use ip address.
// os.Hostname returns just the hostname (automate-deployment) instead of the
// FQDN (automate-deployment.test) which is what we really want.
func LbFQDN() string {
	out, err := command.Output("hostname", command.Args("-f"))
	if err == nil {
		return strings.TrimSpace(out)
	}

	// Grab the last interface that is not loopback. Taken from
	// https://stackoverflow.com/questions/23558425/how-do-i-get-the-local-ip-address-in-go
	addrs, err := net.InterfaceAddrs()
	if err == nil {
		for i := len(addrs) - 1; i >= 0; i-- {
			a := addrs[i]
			if ipnet, ok := a.(*net.IPNet); ok && !ipnet.IP.IsLoopback() {
				if ipnet.IP.To4() != nil {
					return ipnet.IP.String()
				}
			}
		}
	}
	return "localhost"
}

func esHeapSize() string {
	sysMem, err := sys.SystemMemoryKB()
	if err != nil {
		sysMem = 0
	}
	return fmt.Sprintf("%dg", es.RecommendedHeapSizeGB(sysMem))
}

// GeneratePassword generates a random password. This function an be
// used when generating default configuration.
func GeneratePassword() (string, error) {
	bytes := make([]byte, 16)
	if _, err := rand.Read(bytes); err != nil {
		return "", err
	}
	return hex.EncodeToString(bytes), nil
}

func tlsFromUser(cfg *InitConfig, keyPath string, certPath string) error {
	keyData, err := ioutil.ReadFile(keyPath)
	if err != nil {
		return errors.Wrapf(err, "failed to read private key from %s", keyPath)
	}

	certData, err := ioutil.ReadFile(certPath)
	if err != nil {
		return errors.Wrapf(err, "failed to read certificate from %s", certPath)
	}

	cfg.FrontendKey = string(keyData)
	cfg.FrontendCert = string(certData)
	return nil

}

func generateTLS(cfg *InitConfig) error {
	key, cert, err := generateTLSCerts(cfg.Fqdn)
	if err != nil {
		return err
	}

	cfg.TLSWarning = selfSignedTLSWarning
	cfg.FrontendKey = key
	cfg.FrontendCert = cert
	return nil
}

const certLifetime = 10 * 365 * 24 * time.Hour
const keyLength = 2048
const selfSignedTLSWarning = `
    # The following TLS certificate and RSA public key were
    # automatically generated. The certificate is a self-signed
    # certificate and will likely produce security warnings when you
    # visit Chef Automate in your web browser. We recommend using a
    # certificate signed by a certificate authority you trust.`

func generateTLSCerts(fqdn string) (string, string, error) {
	key, err := generatePrivateKey()
	if err != nil {
		return "", "", errors.Wrap(err, "failed to generate private key")
	}

	cert, err := generateCert(key, fqdn)
	if err != nil {
		return "", "", errors.Wrap(err, "failed to generate TLS certificate")
	}

	pemKey, err := pemEncodePrivateKey(key)
	if err != nil {
		return "", "", errors.Wrap(err, "failed to PEM-encode private key")
	}

	pemCert, err := pemEncodeCert(cert)
	if err != nil {
		return "", "", errors.Wrap(err, "failed to PEM-encode certificate")
	}

	return pemKey, pemCert, nil
}

func pemEncode(dType string, data []byte) (string, error) {
	ret := new(strings.Builder)
	err := pem.Encode(ret, &pem.Block{Type: dType, Bytes: data})
	if err != nil {
		return "", err
	}

	return ret.String(), nil
}

func pemEncodePrivateKey(key *rsa.PrivateKey) (string, error) {
	return pemEncode("RSA PRIVATE KEY", x509.MarshalPKCS1PrivateKey(key))
}

func pemEncodeCert(cert []byte) (string, error) {
	return pemEncode("CERTIFICATE", cert)
}

func generatePrivateKey() (*rsa.PrivateKey, error) {
	// TODO(ssd) 2018-04-17: Consider moving to ecdsa
	return rsa.GenerateKey(rand.Reader, keyLength)
}

func generateSerial() (*big.Int, error) {
	// According to
	// https://cabforum.org/wp-content/uploads/CA-Browser-Forum-BR-1.6.4.pdf:
	//
	// Effective September 30, 2016, CAs SHALL generate
	// non-sequential Certificate serial numbers greater than zero
	// (0) containing at least 64 bits of output from a CSPRNG.
	//
	// Here, we set the limit to double this requirement.
	limit := new(big.Int).Lsh(big.NewInt(1), 128)
	ret, err := rand.Int(rand.Reader, limit)
	if err != nil {
		return nil, errors.Wrap(err, "failed to generate serial number")
	}
	return ret, nil
}

func generateCert(priv *rsa.PrivateKey, fqdn string) ([]byte, error) {
	serial, err := generateSerial()
	if err != nil {
		return nil, err
	}

	certSpec := x509.Certificate{
		SerialNumber: serial,
		Subject: pkix.Name{
			Country:            []string{"US"},
			Organization:       []string{"Chef Software"},
			OrganizationalUnit: []string{"Chef Automate"},
			CommonName:         fqdn,
		},
		NotBefore: time.Now(),
		NotAfter:  time.Now().Add(certLifetime),
		// KeyUsageCertSign is here because of the IsCA: true below.
		KeyUsage:    x509.KeyUsageKeyEncipherment | x509.KeyUsageDigitalSignature | x509.KeyUsageCertSign,
		ExtKeyUsage: []x509.ExtKeyUsage{x509.ExtKeyUsageServerAuth, x509.ExtKeyUsageClientAuth},
		// NOTE(ssd) 2018-04-17: This is what A1 does and
		// might be required by some browsers, but I'm not
		// convinced this is correct.
		IsCA:                  true,
		BasicConstraintsValid: true,
	}

	if ip := net.ParseIP(fqdn); ip != nil {
		certSpec.IPAddresses = []net.IP{ip}
	} else {
		certSpec.DNSNames = []string{fqdn}
	}

	cert, err := x509.CreateCertificate(rand.Reader, &certSpec, &certSpec, &priv.PublicKey, priv)
	if err != nil {
		return []byte{}, err
	}

	return cert, nil
}

// Add proxy settings to the config if the user has any of the proxy environment variables set.
func generateProxySettings(c *InitConfig) error {
	possibleProxyEnvVars := [4]string{"https_proxy", "http_proxy", "HTTPS_PROXY", "HTTP_PROXY"}

	proxyStr := ""
	for _, element := range possibleProxyEnvVars {
		if val, ok := os.LookupEnv(element); ok {
			proxyStr = val
			break
		}
	}

	if proxyStr == "" {
		return nil
	}

	proxyURL, err := url.Parse(proxyStr)
	if err != nil {
		return err
	}
	c.ProxyHost = proxyURL.Hostname()
	port := proxyURL.Port()
	if port != "" {
		// Config expects ports to be int32
		p, err := strconv.Atoi(port)
		if err != nil {
			return err
		}
		c.ProxyPort = int32(p)
	}

	userinfo := proxyURL.User
	username := userinfo.Username()
	if username != "" {
		c.ProxyUser = username
	}
	password, maybe := userinfo.Password()
	if maybe {
		c.ProxyPassword = password
	}

	noProxyEnvVars := [2]string{"no_proxy", "NO_PROXY"}

	for _, elt := range noProxyEnvVars {
		if val, ok := os.LookupEnv(elt); ok {
			npEntries := strings.Split(val, ",")
			for i, v := range npEntries {
				npEntries[i] = strings.TrimSpace(v)
			}
			c.NoProxy = npEntries
			break
		}
	}

	// Populate no_proxy with our default entires if they aren't
	// already there.
	for _, hostSpec := range proxy.DefaultNoProxyEntries {
		if !stringutils.SliceContains(c.NoProxy, hostSpec) {
			c.NoProxy = append(c.NoProxy, hostSpec)
		}
	}

	return nil
}
