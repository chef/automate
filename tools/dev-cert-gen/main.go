package main

import (
	"fmt"
	"io/ioutil"
	"net"
	"os"
	"os/exec"
	"path"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/certauthority"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/lib/io/fileutils"
)

var opts = struct {
	Debug   bool
	RegenCA bool
	All     bool
	Force   bool
	DataDir string
}{}

func main() {
	cmd := &cobra.Command{
		Use:           "dev-cert-gen [NAME...]",
		Short:         "Generate a dev certificate and key for the named service",
		SilenceUsage:  true,
		SilenceErrors: true,
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			if opts.Debug {
				logrus.SetLevel(logrus.DebugLevel)
			}
		},
		RunE: run,
	}

	cmd.PersistentFlags().BoolVarP(
		&opts.Debug,
		"debug",
		"d",
		false,
		"Enabled debug output")

	cmd.PersistentFlags().BoolVar(
		&opts.RegenCA,
		"regen-ca",
		false,
		"Regenerate the root CA")

	cmd.PersistentFlags().BoolVar(
		&opts.Force,
		"force",
		false,
		"Generate certificate even if existing certificate exists")

	cmd.PersistentFlags().StringVar(
		&opts.DataDir,
		"data-dir",
		"dev/certs",
		"data directory for dev certs.")

	cmd.PersistentFlags().BoolVarP(
		&opts.All,
		"all",
		"a",
		false,
		"Regenerate all service certifices, creating the CA if necessary")

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}
}

func run(_ *cobra.Command, args []string) error {
	if len(args) < 1 && !opts.All && !opts.RegenCA {
		return errors.New("expected service name argument or one of the following options: --all, --regen-ca")
	}

	_, err := exec.LookPath("certstrap")
	if err != nil {
		return errors.New("could not find certstrap binary in path. This tool requires certstrap to be installed")
	}

	exists, _ := fileutils.PathExists(opts.DataDir)
	if !exists {
		return errors.Errorf("data directory %q does not exist", opts.DataDir)
	}

	ca := certauthority.NewCertstrapCertAuthority(opts.DataDir, "FAKE Dev")
	logrus.Info("Initializing dev certificate authority")
	err = ca.InitAuthority()
	if err != nil {
		return errors.Wrap(err, "could not initialize certificate authority")
	}

	if opts.RegenCA {
		logrus.Info("Regenerating dev certificate authority")
		err := ca.RegenerateRoot()
		if err != nil {
			return errors.Wrap(err, "failed to regenerate CA")
		}
	}

	serviceNames := args
	if opts.All {
		serviceNames, err = allServiceNames()
		if err != nil {
			return errors.Wrap(err, "error getting list of services")
		}
	}

	for _, svc := range serviceNames {
		certsExist := serviceCertsExist(svc)
		if certsExist && !opts.Force {
			logrus.Infof("Certificate already exists for %q and --force not passed, skipping", svc)
			continue
		} else if certsExist {
			logrus.Warnf("Removing existing cert data for %q", svc)
			err := maybeRemoveExisting(svc)
			if err != nil {
				return errors.Wrapf(err, "failed to remove existing cert for %q", svc)
			}
		}
		logrus.Infof("Generating certificate for %q", svc)
		req := certauthority.NewCertRequest(svc, []net.IP{}, []string{})
		certData, err := ca.CertDataForService(req)
		if err != nil {
			return errors.Wrapf(err, "failed to generate cert for %q", svc)
		}

		err = writeCertData(svc, certData)
		if err != nil {
			return errors.Wrapf(err, "failed to write cert data for %q", svc)
		}
	}
	return nil
}

func keyPath(name string) string {
	return path.Join(opts.DataDir, fmt.Sprintf("%s.key", name))
}

func certPath(name string) string {
	return path.Join(opts.DataDir, fmt.Sprintf("%s.crt", name))
}

func writeCertData(name string, data *certauthority.ServiceCertData) error {
	err := ioutil.WriteFile(keyPath(name), []byte(data.Key+"\n"), 0644)
	if err != nil {
		return err
	}
	return ioutil.WriteFile(certPath(name), []byte(data.Cert+"\n"), 0644)
}

func serviceCertsExist(name string) bool {
	keyExists, _ := fileutils.PathExists(keyPath(name))
	certExists, _ := fileutils.PathExists(certPath(name))
	return keyExists || certExists
}

func maybeRemoveExisting(name string) error {
	err := os.Remove(keyPath(name))
	if err != nil && !os.IsNotExist(err) {
		return err
	}

	err = os.Remove(certPath(name))
	if err != nil && !os.IsNotExist(err) {
		return err
	}

	return nil
}

func allServiceNames() ([]string, error) {
	allServices, err := services.AllServices()
	if err != nil {
		return nil, err
	}

	ret := make([]string, len(allServices)+1)
	for i, pkg := range allServices {
		ret[i] = pkg.Name()
	}
	ret[len(ret)-1] = "deployment-service"
	return ret, nil
}
