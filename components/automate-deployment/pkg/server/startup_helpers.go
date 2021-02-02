package server

import (
	"fmt"
	"os"
	"strings"
	"syscall"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/lib/platform/command"
)

// How long to wait for information commands
var logCmdTimeout = 5 * time.Second

func habVersion() string {
	output, err := command.Output("hab",
		command.Args("--version"),
		command.Envvar("HAB_LICENSE", "accept-no-persist"),
		command.Timeout(logCmdTimeout))
	if err != nil {
		return fmt.Sprintf("unknown (hab --version failed with: %s)", output)
	}

	return strings.TrimSpace(output)
}

func habSupVersion() string {
	output, _ := command.Output("hab",
		command.Args("sup", "--version"),
		command.Envvar("HAB_LICENSE", "accept-no-persist"),
		command.Timeout(logCmdTimeout))
	// hab sup --version returns non-zero even when it hasn't errored
	// so for now we can just return the output I guess.
	return strings.TrimSpace(output)
}

// maybeResetHabPath adds the bindir from our manifest's hab package
// to our PATH. This is a development-mode only operation to account
// for test cases where we want to deploy with the latest Habitat.
func maybeResetHabPath(s *server) {
	if os.Getenv("CHEF_DEV_ENVIRONMENT") != "true" {
		return
	}

	if m := s.deployment.CurrentReleaseManifest; m != nil {
		found, habP := m.PackageForServiceName("hab")
		if !found {
			logrus.Warn("No hab package found in manifest")
			return
		}
		habPathEntry := fmt.Sprintf("%s/bin", habpkg.PathFor(&habP))
		existingPath := os.Getenv("PATH")
		if strings.Contains(existingPath, habPathEntry) {
			return
		}

		logrus.Warnf("Adding hab bindir %s to PATH", habPathEntry)
		var newPath string
		if existingPath != "" {
			newPath = strings.Join([]string{habPathEntry, existingPath}, ":")
		} else {
			newPath = habPathEntry
		}
		os.Setenv("PATH", newPath) // nolint:errcheck
	}
}

func setAndLogProcessState() {
	defaultUmask := 022
	oldUmask := syscall.Umask(defaultUmask)
	if oldUmask == defaultUmask {
		logrus.Infof("Umask: %#o", defaultUmask)
	} else {
		logrus.Infof("Changed umask from %#o to %#o", oldUmask, defaultUmask)
	}
	euid := syscall.Geteuid()
	logrus.Infof("hab version: %s", habVersion())
	logrus.Infof("hab-sup version: %s", habSupVersion())
	logrus.Infof("Effective UID: %d", euid)
	logrus.Infof("PATH: %s", os.Getenv("PATH"))
}

func setLogrusLevel(newLogLevel string) {
	var err error
	level := logrus.InfoLevel
	if newLogLevel != "" {
		level, err = logrus.ParseLevel(newLogLevel)
		if err != nil {
			logrus.WithField("level", newLogLevel).WithError(err).Error("could not parse log level using default level of 'info'")
		}
	}
	logrus.SetLevel(level)
}
