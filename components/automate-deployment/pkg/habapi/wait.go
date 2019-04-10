package habapi

import (
	"context"
	"strings"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

const (
	procStateUp   = "up"
	procStateDown = "down"
)

var defaultWaitInterval = 500 * time.Millisecond

// WaitForStopped waits the given duration for all passed services to
// have a process state of down. Unloaded services will also be counted as down.
func WaitForDown(c *Client, services []habpkg.VersionedPackage, timeout time.Duration) error {
	isDown := func(actualSvcs []ServiceInfo, svc habpkg.VersionedPackage) bool {
		if len(actualSvcs) > 1 {
			logrus.Warnf("Found %d services with name %q", len(actualSvcs), svc.Name())
		}
		return allWithProcessState(actualSvcs, procStateDown)
	}

	return waitForAllInExpectedState(c, services, timeout, isDown)
}

// WaitForUp waits the given duration for all passed services to
// be up.
func WaitForUp(c *Client, services []habpkg.VersionedPackage, timeout time.Duration) error {
	isUp := func(actualSvcs []ServiceInfo, svc habpkg.VersionedPackage) bool {
		if len(actualSvcs) == 0 {
			return false
		}

		if len(actualSvcs) > 1 {
			logrus.Warnf("Found %d services with name %q", len(actualSvcs), svc.Name())
		}
		return allWithProcessState(actualSvcs, procStateUp)
	}
	return waitForAllInExpectedState(c, services, timeout, isUp)
}

// WaitForUnload waits for all services to be unloaded. We consider a
// service unloaded if the habitat supervisor doesn't know about it.
func WaitForUnload(c *Client, services []habpkg.VersionedPackage, timeout time.Duration) error {
	// If we get passed a service, it must still be loaded
	isUnloaded := func(actualSvcs []ServiceInfo, svc habpkg.VersionedPackage) bool { return len(actualSvcs) == 0 }
	return waitForAllInExpectedState(c, services, timeout, isUnloaded)
}

// allWithProcessState returns true if all members of services match
// the given process state. Always returns true for an empty array.
func allWithProcessState(services []ServiceInfo, processState string) bool {
	for _, svc := range services {
		// Sometime in the distant past hab returned Up and
		// Down for the process state. We know this because we
		// have saved hab responses with that data.
		if strings.ToLower(svc.Process.State) != processState {
			return false
		}
	}
	return true
}

// waitForAllInExpectedState waits for all of the given services to be
// in the given expected state. The expected state is determined by
// the caller-provided func which is passed the expected services and
// all loaded services that matched that expected service by name.
func waitForAllInExpectedState(
	c *Client,
	services []habpkg.VersionedPackage,
	timeout time.Duration,
	expectedStateFunc func([]ServiceInfo, habpkg.VersionedPackage) bool) error {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	for {
		// NOTE(ssd) 2018-10-04: Kinda wasteful but makes for
		// nicer debug logging.
		notInState := make([]string, 0, len(services))
		loadedServices, err := c.ListServices(ctx)
		if err != nil {
			return err
		}

		for _, expectedSvc := range services {
			actualSvcs := selectByName(loadedServices, expectedSvc.Name())
			inExpectedState := expectedStateFunc(actualSvcs, expectedSvc)
			if !inExpectedState {
				notInState = append(notInState, expectedSvc.Name())
			}
		}

		if len(notInState) == 0 {
			logrus.Debugf("All %d services have entered expected state", len(services))
			return nil
		}

		logrus.Debugf("Still waiting for %d of %d services to enter expected state: %v", len(notInState), len(services), notInState)
		time.Sleep(defaultWaitInterval)
	}
}

func selectByName(services []ServiceInfo, name string) []ServiceInfo {
	ret := make([]ServiceInfo, 0, 1)
	for _, svc := range services {
		if svc.Pkg.Name == name {
			ret = append(ret, svc)
		}
	}

	return ret
}
