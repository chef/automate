package bootstrap

import (
	"context"
	"encoding/json"
	"io/ioutil"
	"os"
	"strings"

	"github.com/golang/protobuf/proto"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/platform/command"
)

type DeploymentServiceCommand struct {
	CmdExecutor command.Executor
	dsPkg       habpkg.VersionedPackage
}

func NewDeploymentServiceCommand(dsPkg habpkg.VersionedPackage, t target.Target) *DeploymentServiceCommand {
	return &DeploymentServiceCommand{
		CmdExecutor: t.CommandExecutor(),
		dsPkg:       dsPkg,
	}
}

var capCache map[string]struct{}

func (ds *DeploymentServiceCommand) HasCapability(capname string) bool {
	if capCache == nil {
		err := ds.populateCapCache()
		if err != nil {
			return false
		}
	}
	_, ok := capCache[capname]
	return ok
}

func (ds *DeploymentServiceCommand) CanBootstrap() bool {
	return ds.HasCapability("bootstrap")
}

func (ds *DeploymentServiceCommand) CanUnpackBootstrapBundle() bool {
	return ds.HasCapability("bootstrap-bundle")
}

func (ds *DeploymentServiceCommand) DeployService(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest, bootstrapBundlePath string) error {
	configFile, err := ds.configToFile(config)
	if err != nil {
		return err
	}
	defer os.Remove(configFile)

	manifestFile, err := ds.manifestToFile(m)
	if err != nil {
		return err
	}
	defer os.Remove(manifestFile)

	args := []string{
		"pkg", "exec", habpkg.Ident(ds.dsPkg), "deployment-service",
		"deploy-service", configFile, manifestFile,
	}

	if bootstrapBundlePath != "" {
		args = append(args, "--bootstrap-bundle-path", bootstrapBundlePath)
	}

	return ds.CmdExecutor.Run("hab",
		command.Args(args...),
		command.Envvar("HAB_LICENSE", "accept-no-persist"),
		command.Envvar("CHEF_AUTOMATE_LOG_LEVEL", logrus.GetLevel().String()),
		command.Stdout(os.Stdout),
		command.Stderr(os.Stderr),
		command.Context(ctx),
	)
}

func (ds *DeploymentServiceCommand) SetupSupervisor(ctx context.Context, config *dc.ConfigRequest, m manifest.ReleaseManifest) error {
	configFile, err := ds.configToFile(config)
	if err != nil {
		return err
	}
	defer os.Remove(configFile)

	manifestFile, err := ds.manifestToFile(m)
	if err != nil {
		return err
	}
	defer os.Remove(manifestFile)

	return ds.CmdExecutor.Run("hab",
		command.Args("pkg", "exec", habpkg.Ident(ds.dsPkg), "deployment-service",
			"setup-supervisor", configFile, manifestFile),
		command.Envvar("HAB_LICENSE", "accept-no-persist"),
		command.Envvar("CHEF_AUTOMATE_LOG_LEVEL", logrus.GetLevel().String()),
		command.Stdout(os.Stdout),
		command.Stderr(os.Stderr),
		command.Context(ctx),
	)
}

func (ds *DeploymentServiceCommand) UnpackBootstrapBundle(ctx context.Context, bundlePath string) error {
	if bundlePath == "" {
		return nil
	}

	return ds.CmdExecutor.Run("hab",
		command.Args("pkg", "exec", habpkg.Ident(ds.dsPkg), "deployment-service",
			"unpack-bootstrap-bundle", bundlePath),
		command.Envvar("HAB_LICENSE", "accept-no-persist"),
		command.Envvar("CHEF_AUTOMATE_LOG_LEVEL", logrus.GetLevel().String()),
		command.Stdout(os.Stdout),
		command.Stderr(os.Stderr),
		command.Context(ctx),
	)
}

func (ds *DeploymentServiceCommand) manifestToFile(m manifest.ReleaseManifest) (string, error) {
	a2, ok := m.(*manifest.A2)
	if !ok {
		return "", errors.New("could not marshal release manifest")
	}
	manifestData, err := json.Marshal(a2)
	if err != nil {
		return "", errors.Wrap(err, "could not marshal release manifest")
	}

	return ds.writeTempFile("", "bootstrapManifest", manifestData)
}

func (ds *DeploymentServiceCommand) configToFile(config *dc.ConfigRequest) (string, error) {
	configData, err := proto.Marshal(config)
	if err != nil {
		return "", errors.Wrap(err, "could not marshal configuration")
	}

	return ds.writeTempFile("", "bootstrapConfig", configData)
}

func (ds *DeploymentServiceCommand) writeTempFile(dir string, prefix string, data []byte) (string, error) {
	tmpFile, err := ioutil.TempFile(dir, prefix)
	if err != nil {
		return "", errors.Wrap(err, "could not create temporary file")
	}

	name := tmpFile.Name()

	_, err = tmpFile.Write(data)
	if err != nil {
		return name, errors.Wrap(err, "could not write configuration to temporary file")
	}

	err = tmpFile.Sync()
	if err != nil {
		return name, errors.Wrap(err, "could not sync temporary file")
	}

	err = tmpFile.Close()
	if err != nil {
		return name, errors.Wrap(err, "failed to close temporary file")
	}

	return name, nil
}

func (ds *DeploymentServiceCommand) populateCapCache() error {
	capList, err := ds.CmdExecutor.Output("hab",
		command.Args("pkg", "exec", habpkg.Ident(ds.dsPkg), "deployment-service", "capabilities"))
	if err != nil {
		return err
	}

	caps := strings.Split(capList, "\n")
	capCache = make(map[string]struct{}, len(caps))

	for _, cap := range caps {
		capCache[cap] = struct{}{}
	}
	return nil
}
