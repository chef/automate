package preflight

import (
	"bytes"
	"fmt"
	"os"
	"reflect"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/automate-grpc/protoc-gen-a2-config/api/a2conf"
	"github.com/chef/automate/lib/proc"
	"github.com/chef/automate/lib/user"
)

func RootUserRequiredCheck() Check {
	return Check{
		Name:        "root_user_required",
		Description: "root user required",
		TestFunc: func(t TestProbe) error {
			if t.Euid() != 0 {
				t.ReportFailure("not running as root")
				return nil
			}
			t.ReportSuccess("running as root")
			return nil
		},
	}
}

const GB = 1 << 30

func minDiskBytesForConfig(c *dc.AutomateConfig) uint64 {
	collections := deployment.CollectionsForConfig(c.GetDeployment())
	minSize := uint64(0)
	if services.ContainsCollection(services.BuilderCollectionName, collections) {
		minSize += 15 * GB
	}
	if services.ContainsCollection(services.AutomateCollectionName, collections) {
		minSize += 5 * GB
	}
	return minSize
}

func DefaultMinimumDiskCheck(c *dc.AutomateConfig) Check {
	return MinimumDiskCheck(minDiskBytesForConfig(c))
}

func MinimumDiskCheck(minimumBytes uint64) Check {
	return Check{
		Name:        "minimum_disk_space",
		Description: "minimum disk space required",
		TestFunc: func(t TestProbe) error {
			spaceAvailable, err := t.AvailableDiskSpace("/hab")
			if err != nil {
				logrus.WithError(err).Debug("Could not get available space at /hab")
				spaceAvailable, err = t.AvailableDiskSpace("/")
				if err != nil {
					return errors.Wrap(err, "could not check available disk space at /")
				}
			}

			msg := fmt.Sprintf("volume: has %.1fGB avail (need %.1fGB for installation)", float64(spaceAvailable)/(1<<30), float64(minimumBytes)/(1<<30))
			if spaceAvailable < minimumBytes {
				t.ReportFailure(msg)
			} else {
				t.ReportSuccess(msg)
			}
			return nil
		},
	}
}

func IsSystemdCheck() Check {
	return Check{
		Name:        "is_systemd",
		Description: "systemd required",
		TestFunc: func(t TestProbe) error {
			bytes, err := t.File("/proc/1/comm")
			if err != nil {
				return errors.Wrap(err, "could not determine init system")
			}

			if !strings.HasPrefix(string(bytes), "systemd") {
				t.ReportFailure("init system is not systemd")
			} else {
				t.ReportSuccess("init system is systemd")
			}
			return nil
		},
	}
}

const a2DeployedSummary = `Chef Automate is already deployed.
To perform a clean install, follow the uninstall instructions at
https://automate.chef.io/docs/troubleshooting/
and run the deploy again.
`

func IsA2DeployedCheck(portCheck *Check) Check {
	return Check{
		Name: "is_a2_deployed",
		TestFunc: func(t TestProbe) error {
			_, err := t.File("/hab/user/deployment-service/config/user.toml")
			if err != nil {
				if os.IsNotExist(err) {
					t.ReportSuccess("automate not already deployed")
					if portCheck != nil {
						return portCheck.TestFunc(t)
					}
					return nil
				}
				return errors.Wrap(err, "could not determine if automate is already deployed")
			}
			t.ReportFailure("automate already deployed")
			t.ReportSummary(a2DeployedSummary)
			return nil
		},
	}
}

const cliInBinSummary = `The chef-automate binary was found in /bin.
The deployment process is responsible for managing this file. To
proceed, move the chef-automate binary to another directory that
is not in the PATH and try again.
`

func CLIInBin() Check {
	return Check{
		Name: "cli_in_bin",
		TestFunc: func(t TestProbe) error {
			reportSuccess := func() {
				t.ReportSuccess("chef-automate CLI is not in /bin")
			}
			isSymlink, err := t.IsSymlink("/bin/chef-automate")
			if err != nil {
				if os.IsNotExist(err) {
					reportSuccess()
					return nil
				}
				return errors.Wrap(err, "failed to check if chef-automate CLI is in /bin")
			}
			if isSymlink {
				reportSuccess()
				return nil
			}
			t.ReportFailure("chef-automate binary is in /bin")
			t.ReportSummary(cliInBinSummary)
			return nil
		},
	}
}

func HasUseraddCheck() Check {
	return HasCmdCheck("useradd")
}

func HasCmdCheck(cmdName string) Check {
	return Check{
		Name: fmt.Sprintf("has_cmd_%s", cmdName),
		TestFunc: func(t TestProbe) error {
			_, err := t.LookPath(cmdName)
			if err != nil {
				logrus.WithError(err).WithField("cmd", cmdName).Debug("failed to find command")
				t.ReportFailure(fmt.Sprintf("required command %q not found", cmdName))
			} else {
				t.ReportSuccess(fmt.Sprintf("found required command %q", cmdName))
			}
			return nil
		},
	}
}

func HasNobodyCheck() Check {
	return HasUserCheck("nobody")
}

func HasUserCheck(username string) Check {
	return Check{
		Name: fmt.Sprintf("has_user_%s", username),
		TestFunc: func(t TestProbe) error {
			_, err := t.LookupUser(username)
			if err != nil {
				if _, ok := err.(user.UnknownUserError); ok {
					t.ReportFailure(fmt.Sprintf("user %q does not exist", username))
					return nil
				}
				return errors.Wrapf(err, "could not determine if user %q exists", username)
			}

			t.ReportSuccess(fmt.Sprintf("user %q exists", username))
			return nil
		},
	}
}

func DefaultMinimumRAMCheck() Check {
	return MinimumRAMCheck(2000000)
}

func MinimumRAMCheck(kiloBytesMinimum int) Check {
	return Check{
		Name: "minimum_ram",
		TestFunc: func(t TestProbe) error {
			data, err := t.File("/proc/meminfo")
			if err != nil {
				return errors.Wrap(err, "could not determine available memory")
			}
			reader := bytes.NewReader(data)
			memTotalKiloBytes, err := proc.ParseMemInfoMemTotal(reader)
			if err != nil {
				logrus.WithError(err).Debug(string(data))
				return errors.Wrap(err, "could not determine available memory")
			}

			memTotalGigByte := float64(memTotalKiloBytes) / 1000000
			memMinimumGigByte := float64(kiloBytesMinimum) / 1000000

			if memTotalKiloBytes < kiloBytesMinimum {
				t.ReportFailure(fmt.Sprintf("MemTotal %d kB (%.1fGB) must be at least %d kB (%.1fGB)", memTotalKiloBytes, memTotalGigByte, kiloBytesMinimum, memMinimumGigByte))
			} else {
				t.ReportSuccess(fmt.Sprintf("MemTotal %d kB (%.1fGB) is at least %d kB (%.1fGB)", memTotalKiloBytes, memTotalGigByte, kiloBytesMinimum, memMinimumGigByte))
			}
			return nil
		},
	}
}

func DefaultKernelVersionCheck() Check {
	return KernelVersionCheck(proc.KernelVersion{
		Major: 3,
		Minor: 2,
	})
}

func KernelVersionCheck(minimumVersion proc.KernelVersion) Check {
	return Check{
		Name: "kernel_version",
		TestFunc: func(t TestProbe) error {
			data, err := t.File("/proc/sys/kernel/osrelease")
			if err != nil {
				return errors.Wrap(err, "could not read kernel version from /proc/sys/kernel/osrelease")
			}
			kernelVersion, err := proc.ParseOSRelease(data)
			if err != nil {
				return errors.Wrap(err, "could not determine kernel version")
			}

			if kernelVersion.Major > minimumVersion.Major || (kernelVersion.Major == minimumVersion.Major && kernelVersion.Minor >= minimumVersion.Minor) {
				t.ReportSuccess(fmt.Sprintf("kernel version %q is at least %q", kernelVersion, minimumVersion))
			} else {
				t.ReportFailure(fmt.Sprintf("kernel version %q must be equal or greater than %q", kernelVersion, minimumVersion))
			}

			return nil
		},
	}
}

var sysctlChecks = []SysctlCheck{
	{
		Name:       "fs.file-max",
		RangeLower: 64000,
	},
	{
		Name:       "vm.max_map_count",
		RangeLower: 262144,
	},
	{
		Name:       "vm.dirty_ratio",
		RangeLower: 5,
		RangeUpper: 30,
		FixValue:   15,
	},
	{
		Name:       "vm.dirty_background_ratio",
		RangeLower: 10,
		RangeUpper: 60,
		FixValue:   35,
	},
	{
		Name:       "vm.dirty_expire_centisecs",
		RangeLower: 10000,
		RangeUpper: 30000,
		FixValue:   20000,
	},
}

func DefaultSysctlCheck() Check {
	return Check{
		Name: "sysctl",
		TestFunc: func(t TestProbe) error {
			failedChecks := []SysctlCheck{}
			for _, c := range sysctlChecks {
				val, err := readSysctl(t, c.Name)
				if err != nil {
					return err
				}

				if c.IsValid(val) {
					if c.IsRange() {
						t.ReportSuccess(fmt.Sprintf("%s=%d is between %d and %d", c.Name, val, c.RangeLower, c.RangeUpper))
					} else {
						t.ReportSuccess(fmt.Sprintf("%s=%d is at least %d", c.Name, val, c.RangeLower))
					}
				} else {
					failedChecks = append(failedChecks, c)
					if c.IsRange() {
						t.ReportFailure(fmt.Sprintf("%s=%d is not between %d and %d", c.Name, val, c.RangeLower, c.RangeUpper))
					} else {
						t.ReportFailure(fmt.Sprintf("%s=%d must be at least %d", c.Name, val, c.RangeLower))
					}
				}
			}

			if len(failedChecks) > 0 {
				builder := strings.Builder{}
				builder.WriteString("Fix the system tuning failures indicated above by running the following:\n")
				for _, c := range failedChecks {
					fmt.Fprintf(&builder, "sysctl -w %s\n", c.SysctlString())
				}

				builder.WriteString("\nTo make these changes permanent, add the following to /etc/sysctl.conf:\n")
				for _, c := range failedChecks {
					fmt.Fprintf(&builder, "%s\n", c.SysctlString())
				}

				t.ReportSummary(builder.String())
			}
			return nil
		},
	}
}

var externalURLs = []string{
	"https://licensing.chef.io/status",
	"https://bldr.habitat.sh",
	"https://raw.githubusercontent.com",
	"https://packages.chef.io",
	"https://github.com",
	"https://downloads.chef.io",
}

const connectivityCheckSummary = `
The following were unreachable and are required for the installation to
successfully complete:
	%s
`

func DefaultConnectivityCheck() Check {
	return ConnectivityCheck(externalURLs)
}

func ConnectivityCheck(urls []string) Check {
	return Check{
		Name: "connectivity_check",
		TestFunc: func(t TestProbe) error {
			failed := []string{}
			for _, u := range urls {
				if err := t.HTTPConnectivity(u); err != nil {
					logrus.WithError(err).WithField("url", u).Debug("Connectivity check failed")
					t.ReportFailure(fmt.Sprintf("%s is not reachable", u))
					failed = append(failed, u)
				} else {
					t.ReportSuccess(fmt.Sprintf("%s is reachable", u))
				}
			}

			if len(failed) > 0 {
				t.ReportSummary(fmt.Sprintf(connectivityCheckSummary, strings.Join(failed, "\n\t")))
			}
			return nil
		},
	}
}

var a1MigratePorts = []uint16{80, 443, 2133, 2134, 5432, 8989, 9611}

func skippablePort(port uint16) bool {
	for _, p := range a1MigratePorts {
		if p == port {
			return true
		}
	}
	return false
}

func requiredPortsForConfig(skipShared bool, config *dc.AutomateConfig) ([]int, error) {
	servicesToCheck, err := deployment.ExpectedServiceIDsForConfig(config.GetDeployment())
	if err != nil {
		return nil, err
	}
	servicesToCheck = append(servicesToCheck, habpkg.New("chef", "deployment-service"))

	portsToCheck := make([]int, 0, len(servicesToCheck)+8)
	// Append habitat ports, currently hard coded nearly everywhere.
	portsToCheck = append(portsToCheck, 9631, 9638)
	acValue := reflect.ValueOf(*config)
	for i := 0; i < acValue.NumField(); i++ {
		f := acValue.Field(i)
		if !f.CanInterface() {
			continue
		}
		if v, ok := f.Interface().(a2conf.A2ServiceConfig); ok {
			if hasServiceByName(servicesToCheck, v.ServiceName()) {
				ports := v.ListPorts()
				for _, p := range ports {
					portVal, err := v.GetPort(p.Name)
					if err != nil {
						return nil, err
					}
					if portVal != 0 && !(skipShared && skippablePort(portVal)) {
						portsToCheck = append(portsToCheck, int(portVal))
					}
				}
			}
		}
	}
	return portsToCheck, nil
}

func hasServiceByName(c []habpkg.HabPkg, name string) bool {
	for _, s := range c {
		if s.Name() == name {
			return true
		}
	}
	return false
}

func DefaultPortCheck(skipShared bool, config *dc.AutomateConfig) (Check, error) {
	portsToCheck, err := requiredPortsForConfig(skipShared, config)
	if err != nil {
		return noopCheck, err
	}
	return PortCheck(portsToCheck), nil
}

func PortCheck(portsToCheck []int) Check {
	return Check{
		Name: "ports_free",
		TestFunc: func(t TestProbe) error {
			usedPorts := make(map[int]bool)

			for _, path := range []string{"/proc/net/tcp", "/proc/net/tcp6"} {
				data, err := t.File(path)
				if err != nil {
					if os.IsNotExist(err) {
						logrus.Debugf("%s was not found", path)
						continue
					}
					return errors.Wrapf(err, "failed to open %s", path)
				}
				ports, err := parseNetTCP(bytes.NewReader(data))
				if err != nil {
					return errors.Wrapf(err, "failed to parse %s", path)
				}
				for _, info := range ports {
					if info.State == TCPStateListen {
						usedPorts[info.LocalPort] = true
					}
				}
			}

			failed := false
			for _, p := range portsToCheck {
				if usedPorts[p] {
					failed = true
					t.ReportFailure(fmt.Sprintf("required port %d in use", p))
				}
			}

			if !failed {
				t.ReportSuccess("initial required ports are available")
			}

			return nil
		},
	}
}
