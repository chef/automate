package main

import (
	"bufio"
	"bytes"
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
	fakeCert = `
-----BEGIN CERTIFICATE-----
MIICczCCAfqgAwIBAgIJAO2zWnZsAUXPMAoGCCqGSM49BAMCMHgxCzAJBgNVBAYT
AlVTMRMwEQYDVQQIDApXYXNoaW5ndG9uMRAwDgYDVQQHDAdTZWF0dGxlMQ0wCwYD
VQQKDARDaGVmMRgwFgYDVQQLDA9EZXBsb3ltZW50IFRlYW0xGTAXBgNVBAMMEHBh
Y2thZ2VzLmNoZWYuaW8wHhcNMTgwNjI1MDkzMDU2WhcNMjgwNjIyMDkzMDU2WjB4
MQswCQYDVQQGEwJVUzETMBEGA1UECAwKV2FzaGluZ3RvbjEQMA4GA1UEBwwHU2Vh
dHRsZTENMAsGA1UECgwEQ2hlZjEYMBYGA1UECwwPRGVwbG95bWVudCBUZWFtMRkw
FwYDVQQDDBBwYWNrYWdlcy5jaGVmLmlvMHYwEAYHKoZIzj0CAQYFK4EEACIDYgAE
+P9L+/ecS1ivfQNp8i5wI/1DdrFWJgI2oeKmH4JesopktIDevPuJhYqF9/2UmzQj
hXT72oIND4KM3njp8tPVs1J0IVTk1C4hxA5t+wNy3MWMbnEVBHR7h7bCCXMJGS2O
o1AwTjAdBgNVHQ4EFgQURoLZEPwhyqyK2I2R1+EEJxRGOBcwHwYDVR0jBBgwFoAU
RoLZEPwhyqyK2I2R1+EEJxRGOBcwDAYDVR0TBAUwAwEB/zAKBggqhkjOPQQDAgNn
ADBkAjBWD6LSmjbA6z1yS3fBnQiHyqJaBck0LHZJEseBRS/R9uWpYaaKXEDDFyM7
jE/u8o4CMFpDgh9IfGwoB4G35Brr3j0Wxo9sxA9Mz350CBgTF4HdKvTkW7+yeNhW
wMZgiK6Dmw==
-----END CERTIFICATE-----
`
	fakeKey = `
-----BEGIN EC PARAMETERS-----
BgUrgQQAIg==
-----END EC PARAMETERS-----
-----BEGIN EC PRIVATE KEY-----
MIGkAgEBBDDioJGejyBR+NEsU2naFoaUqRL30vjT0WyL+Fk/4+VxpXsDFFOONCDE
jvdMc7IP+JegBwYFK4EEACKhZANiAAT4/0v795xLWK99A2nyLnAj/UN2sVYmAjah
4qYfgl6yimS0gN68+4mFioX3/ZSbNCOFdPvagg0PgozeeOny09WzUnQhVOTULiHE
Dm37A3LcxYxucRUEdHuHtsIJcwkZLY4=
-----END EC PRIVATE KEY-----
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
		Use:   "serve MANIFEST_FILE PID_FILE",
		Short: "Start small HTTPS server to serve the given manifest.",
		Run:   serve,
		Args:  cobra.ExactArgs(2),
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
	dl := airgap.NewBintrayHabDownloader()
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
