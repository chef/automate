package server

import (
	"fmt"
	"os"
	"strings"
	"syscall"
	"time"

	"github.com/sirupsen/logrus"

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
