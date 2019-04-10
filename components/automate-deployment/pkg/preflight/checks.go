package preflight

import (
	"bytes"
	"fmt"
	"os"
	"os/user"
	"strings"

	"github.com/chef/automate/lib/proc"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
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

func DefaultMinimumDiskCheck() Check {
	return MinimumDiskCheck(5 * (1 << 30))
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
	"https://api.bintray.com",
	"https://raw.githubusercontent.com",
	"https://packages.chef.io",
	"https://akamai.bintray.com",
	"https://dl.bintray.com",
	"https://bintray.com",
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

type PortToCheck struct {
	Description string
	Port        int
	Shared      bool
}

var defaultRequiredPorts = []PortToCheck{
	{
		Description: "automate-gateway HTTP",
		Port:        2000,
	},
	{
		Description: "automate-gateway GRPC",
		Port:        2001,
	},
	{
		Description: "ingest-service",
		Port:        2192,
	},
	{
		Description: "hab-sup HTTP",
		Port:        9631,
	},
	{
		Description: "hab-sup gossip",
		Port:        9638,
	},
	{
		Description: "pg-sidecar-service",
		Port:        10100,
	},
	{
		Description: "authn-service",
		Port:        10113,
	},
	{
		Description: "UNUSED",
		Port:        10114,
	},
	{
		Description: "session-service",
		Port:        10115,
	},
	{
		Description: "dex",
		Port:        10116,
	},
	{
		Description: "dex",
		Port:        10117,
	},
	{
		Description: "UNUSED",
		Port:        10118,
	},
	{
		Description: "config-mgmt-service",
		Port:        10119,
	},
	{
		Description: "nodemanager-service",
		Port:        10120,
	},
	{
		Description: "compliance-service",
		Port:        10121,
	},
	{
		Description: "ingest-service",
		Port:        10122,
	},
	{
		Description: "es-sidecar-service",
		Port:        10123,
	},
	{
		Description: "license-control-service",
		Port:        10124,
	},
	{
		Description: "notifications-service",
		Port:        10125,
	},
	{
		Description: "local-user-service",
		Port:        10127,
	},
	{
		Description: "teams-service",
		Port:        10128,
	},
	{
		Description: "secrets-service",
		Port:        10131,
	},
	{
		Description: "applications-service",
		Port:        10133,
	},
	// TODO @afiune Uncomment once this is not a feature flag
	//{
	//Description: "event-service NATS",
	//Port:        10140,
	//},
	// @afiune Why is the event-service not here?
	// TODO: Add it to the checks!
	{
		Description: "elasticsearch",
		Port:        10141,
	},
	{
		Description: "elasticsearch",
		Port:        10142,
	},
	{
		Description: "UNUSED",
		Port:        10159,
	},
	{
		Description: "deployment-service",
		Port:        10160,
	},
	{
		Description: "automate-ui",
		Port:        10161,
	},
	{
		Description: "authn-service",
		Port:        10162,
	},
	{
		Description: "automate-load-balancer",
		Port:        80,
		Shared:      true,
	},
	{
		Description: "automate-load-balancer",
		Port:        443,
		Shared:      true,
	},
	{
		Description: "compliance-service",
		Port:        2133,
		Shared:      true,
	},
	{
		Description: "compliance-service",
		Port:        2134,
		Shared:      true,
	},
	{
		Description: "postgresql",
		Port:        5432,
		Shared:      true,
	},
}

func DefaultPortCheck(skipShared bool) Check {
	return PortCheck(defaultRequiredPorts, skipShared)
}

func PortCheck(portsToCheck []PortToCheck, skipShared bool) Check {
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
				if skipShared && p.Shared {
					logrus.Debugf("skipping shared port check %s(%d)", p.Description, p.Port)
					continue
				}
				if usedPorts[p.Port] {
					failed = true
					t.ReportFailure(fmt.Sprintf("required port %d in use", p.Port))
				}
			}

			if !failed {
				t.ReportSuccess("initial required ports are available")
			}

			return nil
		},
	}
}
