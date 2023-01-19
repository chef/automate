package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/platform/sys"
)

const (
	manifestAddr  = "127.0.0.2"
	manifestPort  = 443
	EtcHostsPath  = "/etc/hosts"
	EtcHostsEntry = "127.0.0.2       packages.chef.io\n"
	// Where we need to install our SSL Cert for golang to accept
	// it. This may change based on the version of go used to
	// compile automate.
	sslCertPath = "/etc/ssl/certs"
	sslCertName = "FakePackagesChefIoCert.pem"
	// Hard-coded self-signed certs for package.chef.io to fake out
	// the manifest fetcher.
	fakeCert = `-----BEGIN CERTIFICATE-----
MIIDhDCCAmygAwIBAgIJAJsxkBGqB+OrMA0GCSqGSIb3DQEBCwUAMHExCzAJBgNV
BAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMQ0w
CwYDVQQKDARDaGVmMREwDwYDVQQLDAhEZXYgVGVhbTEZMBcGA1UEAwwQcGFja2Fn
ZXMuY2hlZi5pbzAeFw0yMzAxMTkxMzU4MDBaFw00MzAxMTQxMzU4MDBaMHExCzAJ
BgNVBAYTAlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxl
MQ0wCwYDVQQKDARDaGVmMREwDwYDVQQLDAhEZXYgVGVhbTEZMBcGA1UEAwwQcGFj
a2FnZXMuY2hlZi5pbzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAK8e
CrmOf4hK7mgMSnZkheK20QD0oFyoAHbkmstFeL5Oh/FQVqPnbpTHIzV6CcEiVLSS
gdF8XQMeTiDh39pYdwjglKfShJN9qWuaCfhf3xEdDCZ+XAzWQfp0XwG3WFtker7U
BaFbenWLap+WPOX4+5xpM+m78BgxX4y7UnUff+6RwM2Z6KZpoXQKVn6htgAd1UMs
WYbYtYy88zLyQq0ttSTWuzRxXfOSli8P0bb2QJo/w3SDFoN8jgOny3zjxRJq7Xgy
kplUDG2qny02buSdals2mYZqU5C8YTon0MdNTGgKSmea7nRDbXlQfC9VNxm6vkgp
cv0yc9TcZK+yMU27hYUCAwEAAaMfMB0wGwYDVR0RBBQwEoIQcGFja2FnZXMuY2hl
Zi5pbzANBgkqhkiG9w0BAQsFAAOCAQEAgP+kT1gxk4O499/u0Zr/WlbfLB8oBSPM
jy8R6mj4TqNqT9RzXZm18iWPjl4DOZpvWWg6iZvHpS9pA9xblNCPQfp6h1nJWKwr
mXWHx2d15QqpgsYezyDn05DKFNfQnpt0dgNTksZ6GqZnnb2IePf30DOmF1/UOaXD
N0nGiqhHLKGZR7bq5R+KCbOw/6vZ6DqYpN6DrgtftpFLBNOSzWwHpyXqtayAsfeR
U9I9cWwIrWeNQgEBQZNDkfFPDutVcqpRg8Zj1UE/3raUJHjsfiokD5nUK7V5tMOe
Z1cacR0H0Ee1AXqk9l4qTsqn0cO8fIMdp1w5M8eSnY+waCB0Mi/9tw==
-----END CERTIFICATE-----`
	fakeKey = `
-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCvHgq5jn+ISu5o
DEp2ZIXittEA9KBcqAB25JrLRXi+TofxUFaj526UxyM1egnBIlS0koHRfF0DHk4g
4d/aWHcI4JSn0oSTfalrmgn4X98RHQwmflwM1kH6dF8Bt1hbZHq+1AWhW3p1i2qf
ljzl+PucaTPpu/AYMV+Mu1J1H3/ukcDNmeimaaF0ClZ+obYAHdVDLFmG2LWMvPMy
8kKtLbUk1rs0cV3zkpYvD9G29kCaP8N0gxaDfI4Dp8t848USau14MpKZVAxtqp8t
Nm7knWpbNpmGalOQvGE6J9DHTUxoCkpnmu50Q215UHwvVTcZur5IKXL9MnPU3GSv
sjFNu4WFAgMBAAECggEBAIbfsIryAa/LZouky8Ub7zKKc7c0sj4q9xCbSc29mB2b
BoEJy3E50qWsKqYJdHHYKz1Yibv6+Php4urYjTzyXdvsfRI0xf+DDXXekkdGL9xS
bwVG1UbdzrtD3djIibj6Tr+jNSLt/geCdYCsigLmHYR+Uol3QpifjmQp8nxa4oGv
YoRSPEzmmfGNlK19r06t/KvUIpIvwmks3O2IMRX9sryg3p8IKNXw55TrZzzcJANk
n0A/1CP5oAcW8gx78zV3xwa0qlp9UlVMUrJ8+Gz94Zm/9OLK9zSWtR2Sj7/khghZ
j0iqbwQpeHJ78KHNLlHXcAHumqhhHTNwFY74dPOK/RECgYEA2AZhl5RfElw1WJBy
wO3yt2DTVRatjG13kg54NdRurfXuu49RLcPzhMn2tvtEFBnhDGOYdZmAGj3Y29EW
Tn5CRlP5r0NYKY3Ash4/TbnBEDHCSld067C2lCPwvy3A0ftjjF0U2iGlvTsMP3gq
s8B1/aATqNwFvDDwJKdQKz0IPc8CgYEAz4XFTqkktu+LcNHUqAo1m7wVJUVUyUhB
EWcnbcQpREaFrAXmeQaIGHIL1gUtnXDY6OgQPQrxf0/yZFLQcmmptyfQhunGgd6u
3vcRsMnbhw8DPLqNpglL9vpNURXTIKu+3YPZzg5eVFgMmELkkDctfpNHH85il1xy
GQvSSqe3UGsCgYEAvBYKQLUBdfXsyJi0IUDMFFfsiOg+4Maq1fNdPNkiKLq5SnrF
Hoi+7T3/XmA1bR7/MA9DPhkRTUfOGc3ZkywhPiR9f9it2Tg/r5XgXic+boA0fw/Y
kCGDRkZopbwLZ4huq1acAjRLnw/bYSOaYnlPAi2vEFXJdIpTfEZk6dCw7UcCgYBN
x1JrKAQFuIRntY1hqenrw7OdM34Srs0Ma8AgaNwapd/l2JRIGgWaO+xpu6kcEDaM
HKYxas+Wqu0rYsqThzy/0+FZH3dyJ3cdRDbSLuXjmeHZugXrDCL3n1qpIGfwH866
kXdpvoLmU9Xsn22xC602epa2uhDDsDdnYcHgjzmqnwKBgApW7wXynn47jj8zXVuP
ij7ko2WZah2c66RgqoBR/s7sqvhXglKu1tnAHHkMg0TjfEB4J8BNwtIGRlqPGP8L
o8iVNiNopAjIfOZEcRRUT9tOnNezK7VnbVTu6H9CBu5bJjFHqdOKlNfsDcLgIgeU
QjManutdDNzC5OssD5Xfgb4t
-----END PRIVATE KEY-----
`
)

func main() {
	sys.SetUmask(022)

	rootCmd := &cobra.Command{
		Use:          "upgrade-test-scaffold",
		Short:        "Test setup and scaffolding for A2 upgrade tests",
		Long:         "Test setup and scaffolding for A2 upgrade tests. Useful for testing upgrades from very old A2 versions",
		SilenceUsage: false,
	}

	setupCmd := &cobra.Command{
		Use:   "setup MANIFEST_FILE",
		Short: "Setup host machine for upgrade tests. WARNING: This command modifies system configuration.",
		Long: `Setup host machine for upgrade tests.

This command will install the required chef-automate version from the
manifest., modify /etc/hosts, and set the SSL configuration required
for the 'serve' command to work as expected.
`,
		Run:  setupHost,
		Args: cobra.ExactArgs(1),
	}

	serveCmd := &cobra.Command{
		Use:   "serve MANIFEST_FILE PID_FILE [VERSION_FILE_PATH]",
		Short: "Start small HTTPS server to serve the given manifest.",
		Run:   serve,
		Args:  cobra.RangeArgs(2, 3),
	}

	rootCmd.AddCommand(setupCmd)
	rootCmd.AddCommand(serveCmd)
	err := rootCmd.Execute()
	if err != nil {
		logrus.Fatalf("unhandled error: %s", err.Error())
	}
}

func setupHost(cmd *cobra.Command, args []string) {
	manifest := args[0]

	err := InstallChefAutomate(manifest)
	if err != nil {
		logrus.Fatalf("installing chef-automate: %s", err.Error())
	}

	err = ModifyEtcHosts()
	if err != nil {
		logrus.Fatalf("modifying etc hosts: %s", err.Error())
	}

	err = InstallCerts()
	if err != nil {
		logrus.Fatalf("installing certs: %s", err.Error())
	}
}

func writePidFile(pidFile string) error {
	pidString := strconv.Itoa(os.Getpid())
	return ioutil.WriteFile(pidFile, []byte(pidString), 0700)
}

func serve(cmd *cobra.Command, args []string) {
	manifestPath := args[0]
	pidFile := args[1]
	var versionPath string
	if len(args) >= 3 {
		versionPath = args[2]
	}
	logrus.Info("======== DEEP UPGRADE SCAFFOLD =======")
	logrus.Infof("Using manifest file %s", manifestPath)
	logrus.Infof("Using pid file %s", pidFile)
	logrus.Infof("PID: %d", os.Getpid())

	logrus.Info("Writing PID file")
	err := writePidFile(pidFile)
	if err != nil {
		logrus.Fatalf("creating pidfile: %s", err.Error())
	}

	ch := make(chan os.Signal, 1)
	signal.Notify(ch, syscall.SIGTERM, syscall.SIGINT)
	go func() {
		sig := <-ch
		logrus.WithField("signal", sig).Info("Cleaning up PIDFILE and exiting")
		os.Remove(pidFile)
		os.Exit(0)
	}()

	// The ListenAndServeTLS function takes the certs as paths to files
	sslCert, err := ioutil.TempFile("", "deep_upgrade_scaffold")
	if err != nil {
		logrus.Fatalf("creating tmpfile for ssl cert: %s", err.Error())
	}

	defer os.Remove(sslCert.Name())

	_, err = sslCert.Write([]byte(fakeCert))
	if err != nil {
		logrus.Fatalf("writing ssl cert: %s", err.Error())
	}
	sslCert.Sync() // nolint: errcheck

	sslKey, err := ioutil.TempFile("", "deep_upgrade_scaffold")
	if err != nil {
		logrus.Fatalf("writing tmpfile for ssl key: %s", err.Error())
	}
	defer os.Remove(sslKey.Name())

	_, err = sslKey.Write([]byte(fakeKey))
	if err != nil {
		logrus.Fatalf("writing ssl key: %s", err.Error())
	}
	sslKey.Sync() // nolint: errcheck

	addr := fmt.Sprintf("%s:%d", manifestAddr, manifestPort)
	logrus.Infof("Starting simple HTTPS server to serve manifest on %s", addr)

	manifestMap := make(map[string]string)

	http.HandleFunc("/manifests/current/automate/latest.json", ServeLatestManifest(manifestPath, manifestMap))
	http.HandleFunc("/manifests/dev/automate/latest.json", ServeLatestManifest(manifestPath, manifestMap))
	http.HandleFunc("/manifests/current/automate/latest_semver.json", ServeLatestManifest(manifestPath, manifestMap))
	http.HandleFunc("/manifests/dev/automate/latest_semver.json", ServeLatestManifest(manifestPath, manifestMap))
	http.HandleFunc("/manifests/current/automate/versions.json", ServeVersions(versionPath, manifestMap))
	http.HandleFunc("/manifests/dev/automate/versions.json", ServeVersions(versionPath, manifestMap))
	http.HandleFunc("/manifests/automate/", ServeManifest("/manifests/automate/", manifestMap))

	http.HandleFunc("/set/", func(w http.ResponseWriter, req *http.Request) {
		release := strings.TrimPrefix(req.RequestURI, "/set/")
		if req.Body == nil {
			logrus.WithField("endpoint", req.RequestURI).Error("No body provided")
			w.WriteHeader(400)
			return
		}
		defer req.Body.Close()
		if req.Method != "POST" {
			logrus.WithField("endpoint", req.RequestURI).Error("Expected POST method")
			w.WriteHeader(400)
			return
		}

		if len(release) == 0 {
			logrus.WithField("endpoint", req.RequestURI).Error("Empty release")
			w.WriteHeader(400)
		}

		manifest, err := ioutil.ReadAll(req.Body)
		if err != nil {
			logrus.WithError(err).Error("Failed to write")
			return
		}
		manifestMap[release] = string(manifest)
	})
	err = http.ListenAndServeTLS(addr, sslCert.Name(), sslKey.Name(), nil)
	if err != nil {
		logrus.Fatalf("listen: %s", err.Error())
	}
}

func InstallChefAutomate(manifestPath string) error {
	manifestData, err := ioutil.ReadFile(manifestPath)
	if err != nil {
		return errors.Wrap(err, "reading manifest file")
	}

	manifest, err := parser.ManifestFromBytes(manifestData)
	if err != nil {
		return errors.Wrap(err, "parsing manifest file")
	}

	found, acPkg := manifest.PackageForServiceName("automate-cli")
	if !found {
		return errors.New("automate-cli not found in manifest")
	}

	found, habPkg := manifest.PackageForServiceName("hab")
	if !found {
		return errors.New("hab not found in manifest")
	}

	tmpHabBin, err := ioutil.TempFile("./", "upgrade-scaffold")
	if err != nil {
		return errors.Wrap(err, "creating temp file")
	}

	defer os.Remove(tmpHabBin.Name())

	err = os.Chmod(tmpHabBin.Name(), 0700)
	if err != nil {
		return errors.Wrap(err, "chowning temporary hab bin")
	}

	logrus.Infof("Downloading temporary habitat binary (%s)", habpkg.Ident(&habPkg))
	dl := airgap.NewNetHabDownloader()
	err = dl.DownloadHabBinary(habPkg.Version(), habPkg.Release(), tmpHabBin)
	if err != nil {
		return errors.Wrap(err, "downloading temporary hab")
	}

	err = tmpHabBin.Sync()
	if err != nil {
		return errors.Wrap(err, "sync")
	}

	err = tmpHabBin.Close()
	if err != nil {
		return errors.Wrap(err, "sync")
	}

	habPath, err := filepath.Abs(tmpHabBin.Name())
	if err != nil {
		return errors.Wrap(err, "determining absolute path to temporary hab bin")
	}

	ident := habpkg.Ident(&acPkg)
	logrus.Infof("Installing %s", ident)
	err = command.Run(habPath,
		command.Args("pkg", "install", ident),
		command.Stderr(os.Stderr),
		command.Stdout(os.Stdout))
	if err != nil {
		return errors.Wrap(err, "installing automate-cli failed")
	}

	logrus.Infof("Binlinking %s", ident)
	err = command.Run(habPath,
		command.Args("pkg", "binlink", ident),
		command.Stderr(os.Stderr),
		command.Stdout(os.Stdout))
	if err != nil {
		return errors.Wrap(err, "binlinking automate-cli failed")
	}

	return nil
}

func InstallCerts() error {
	cert := filepath.Join(sslCertPath, sslCertName)
	_, err := os.Stat(cert)
	if os.IsNotExist(err) {
		logrus.Info("Installing fake certificate for packages.chef.io")
		err := ioutil.WriteFile(cert, []byte(fakeCert), 0644)
		return errors.Wrap(err, "WriteFile")
	}

	if err != nil {
		return errors.Wrap(err, "Stat")
	}

	logrus.Info("Fake certificate already installed")
	return nil
}

func ModifyEtcHosts() error {
	etcHosts, err := ioutil.ReadFile(EtcHostsPath)
	if err != nil {
		return errors.Wrap(err, "ReadFile")
	}

	r := bufio.NewReader(bytes.NewReader(etcHosts))
	for {
		line, err := r.ReadString('\n')
		if err == io.EOF {
			// If we get to the end we haven't found the entry yet.
			logrus.Infof("No existing host entry, rewriting %s", EtcHostsPath)
			// NOTE(ssd) 2018-06-27: I originally was
			// doing this with an "AtomicWrite", but that
			// method uses a rename() call which doesn't
			// work in docker, so let's try this instead.
			buf := bytes.Buffer{}
			buf.Write(etcHosts)
			buf.WriteString(EtcHostsEntry)
			return ioutil.WriteFile(EtcHostsPath, buf.Bytes(), 0600)
		}

		if line == EtcHostsEntry {
			logrus.Info("Hosts entry already installed")
			return nil
		}
	}
}

func ServeLatestManifest(manifestPath string, manifests map[string]string) func(w http.ResponseWriter, req *http.Request) {
	return func(w http.ResponseWriter, req *http.Request) {
		if manifest, exists := manifests["latest"]; exists {
			w.Header().Set("Content-Type", "application/json")
			_, err := io.WriteString(w, manifest)
			if err != nil {
				logrus.WithError(err).Error("error copying manifest to client")
			}
			return
		}

		f, err := os.Open(manifestPath)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			w.Write([]byte(err.Error())) // nolint: errcheck
			return
		}

		w.Header().Set("Content-Type", "application/json")
		_, err = io.Copy(w, f)
		if err != nil {
			logrus.WithError(err).Error("error copying manifest to client")
		}
	}
}

func ServeManifest(prefix string, manifests map[string]string) func(w http.ResponseWriter, req *http.Request) {
	return func(w http.ResponseWriter, req *http.Request) {
		release := strings.TrimPrefix(req.RequestURI, prefix)
		release = strings.TrimSuffix(release, ".json")
		if manifest, exists := manifests[release]; exists {
			w.Header().Set("Content-Type", "application/json")
			_, err := io.WriteString(w, manifest)
			if err != nil {
				logrus.WithError(err).Error("error copying manifest to client")
			}
			return
		}
		http.NotFound(w, req)
	}
}

func ServeVersions(versionPath string, manifests map[string]string) func(w http.ResponseWriter, req *http.Request) {
	return func(w http.ResponseWriter, req *http.Request) {
		logrus.Error("Print manifest values:")
		versions := []string{}
		b, err := ioutil.ReadFile(versionPath) // nosemgrep
		if err == nil {
			err = json.Unmarshal(b, &versions)
			if err != nil {
				logrus.WithError(err).Error("could not read versions file", versionPath)
			}
		}
		if len(manifests) > 0 {
			for k := range manifests {
				versions = append(versions, k)
				logrus.Error(fmt.Sprintf("#%v", k))
			}
		}
		w.Header().Set("Content-Type", "application/json")
		versionByte, err := json.Marshal(versions)
		if err != nil {
			logrus.WithError(err).Error("error marshaling versions")
		}
		_, err = io.WriteString(w, string(versionByte)) // nosemgrep
		if err != nil {
			logrus.WithError(err).Error("error copying manifest to client")
		}
	}
}
