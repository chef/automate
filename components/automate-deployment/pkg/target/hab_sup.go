package target

import (
	"context"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/lib/proc"
)

// defaultLauncherPidFile is the location where hab stores the launcher
// pid. We expose this for testing
var defaultLauncherPidFile = "/hab/sup/default/LOCK"

// defaultProcMount is exposed for testing
var defaultProcMount = "/proc"

var defaultHabSupHupWaitTime = 60 * time.Second

var minimumHabSupSighupFeature = habpkg.NewFQ("core", "hab-sup", "0.63.0", "20180914030447")

// HabSup interface describes various operations on hab-sup and launcher
type HabSup interface {
	SupPkg() (habpkg.HabPkg, error)
	SupPid() (int, error)
	LauncherPid() (int, error)
	Hup(ctx context.Context) error
}

// SupportsSupHup returns true if the provided hab-sup version supports
// restarting hab-sup via SIGHUP
func SupportsSupHup(habSupP habpkg.HabPkg) bool {
	return habpkg.GreaterOrEqual(&habSupP, &minimumHabSupSighupFeature)
}

// SupportsCleanShutdown returns true if the provided hab-sup version supports customized service shutdown behavior.
func SupportsCleanShutdown(habSupP habpkg.HabPkg) bool {
	return false
}

type localHabSup struct {
	client habapi.HabServiceInfoAPIClient
}

// LocalHabSup is a HabSup implementation on a local target
func LocalHabSup(client habapi.HabServiceInfoAPIClient) HabSup {
	return localHabSup{
		client: client,
	}
}

func (sup localHabSup) SupPkg() (habpkg.HabPkg, error) {
	response, err := sup.client.ServiceInfo(context.Background(), "deployment-service", "default")
	if err != nil {
		return habpkg.HabPkg{}, errors.Wrap(err, "failed to query hab-sup package version")
	}

	verString := response.Sys.Version
	versionParts := strings.Split(verString, "/")
	if len(versionParts) < 2 {
		return habpkg.HabPkg{}, errors.Errorf("unable to parse version of running hab-sup: %s", verString)
	}

	return habpkg.NewFQ("core", "hab-sup", versionParts[0], versionParts[1]), nil
}

func (sup localHabSup) LauncherPid() (int, error) {
	data, err := ioutil.ReadFile(defaultLauncherPidFile)
	if err != nil {
		return -1, errors.Wrapf(err, "could not access launcher pid file (%s)", defaultLauncherPidFile)
	}

	pid, err := strconv.ParseInt(string(data), 10, 32)
	if err != nil {
		return -1, errors.Wrapf(err, "failed to parse pid %s", string(data))
	}

	return int(pid), nil
}

func (sup localHabSup) SupPid() (int, error) {
	launcherPid, err := sup.LauncherPid()
	if err != nil {
		return -1, errors.Wrap(err, "failed to get launcher pid")
	}

	procTree, err := proc.Tree(proc.WithProcMount(defaultProcMount))
	if err != nil {
		return -1, errors.Wrap(err, "failed to query proc mount")
	}

	launcherProc, exists := procTree[launcherPid]
	if !exists {
		return -1, errors.Errorf("launcher pid %d not found in %s",
			launcherPid, defaultProcMount)
	}

	for _, childProc := range launcherProc.Children {
		stat, err := childProc.Stat()
		if err != nil {
			// Lot's of things can happen from when we list the procs
			// and we try to read its stat file. Errors are expected
			logrus.
				WithError(err).
				WithField("pid", childProc.Pid).
				Warn("Failed to get process stat")
			continue
		}
		if stat.Comm == "hab-sup" {
			return stat.Pid, nil
		}
	}

	return -1, errors.New("could not find hab-sup pid")
}

// Hup sends a SIGHUP to hab-sup and waits for it to be restarted
func (sup localHabSup) Hup(ctx context.Context) error {
	pid, err := sup.SupPid()
	if err != nil {
		return errors.Wrap(err, "failed to find hab-sup pid")
	}

	supProcess, err := os.FindProcess(pid)
	if err != nil {
		return errors.Wrap(err, "failed to find hab-sup process")
	}

	logrus.
		WithField("pid", pid).
		Info("Sending SIGHUP to hab-sup")

	if err := supProcess.Signal(syscall.SIGHUP); err != nil {
		return errors.Wrap(err, "failed to SIGHUP hab-sup")
	}

	timeout, cancel := context.WithTimeout(ctx, defaultHabSupHupWaitTime)
	defer cancel()
	for {
		newPid, err := sup.SupPid()
		if err != nil {
			logrus.WithError(err).Debug("Failed to get hab-sup pid")
		} else {
			logrus.WithFields(logrus.Fields{
				"currentPid": pid,
				"newPid":     newPid,
			}).Debug("Found hab-sup pid")
			if newPid != pid {
				return nil
			}
		}
		select {
		case <-time.After(time.Second):
			logrus.Info("Waiting for new hab-sup pid")
		case <-timeout.Done():
			return errors.Wrap(err, "timed out waiting for hab-sup to restart")
		}
	}
}
