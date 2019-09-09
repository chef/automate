package main

import (
	"bufio"
	"bytes"
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"regexp"
	"strconv"
	"syscall"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/gatherlogs"
	"github.com/chef/automate/lib/platform/command"

	"github.com/chef/automate/lib/io/fileutils"
)

const (
	defaultNginxMasterPidFile  = "/hab/svc/automate-load-balancer/PID"
	defaultNginxClientBodyPath = "/hab/svc/automate-load-balancer/var/client-body"
	defaultNginxConfPath       = "/hab/svc/automate-load-balancer/config/nginx.conf"
)

// TODO: Consider having this generated from a config or dynamically
// in some other way. These are the correct binpaths for
// enterprise-linux
var binPaths = map[string]string{
	"cp":   "/bin/cp",
	"find": "/bin/find",
	"tar":  "/bin/tar",
}

func newGatherLogsCmd() *cobra.Command {
	gatherLogsCmd := &cobra.Command{
		Use:   "gather-logs [/path/to/log/bundle.tar.gz]",
		Short: "Gather system diagnostics and logs",
		Long:  "Collect system diagnostics and logs from Chef Automate and other services",
		RunE:  runGatherLogsCmd,
		Args:  cobra.RangeArgs(0, 1),
	}

	gatherLogsCmd.Flags().BoolVarP(
		&gatherLogsCmdFlags.localFallback,
		"local-fallback",
		"l",
		false,
		"run gather-logs in local fallback mode",
	)

	gatherLogsCmd.Flags().Uint64Var(
		&gatherLogsCmdFlags.logLines,
		"log-lines",
		gatherlogs.DefaultLogLines,
		"Number of system log lines (journald logs) to collect (0 for all logs)",
	)

	gatherLogsCmd.Flags().BoolVarP(
		&gatherLogsCmdFlags.overwrite,
		"overwrite",
		"o",
		false,
		"Overwrite existing log archive",
	)

	gatherLogsCmd.Flags().BoolVarP(
		&gatherLogsCmdFlags.captureData,
		"capture-data",
		"c",
		false,
		"Capture incoming HTTP data (use only by request from Chef Support)",
	)

	gatherLogsCmd.Flags().IntVarP(
		&gatherLogsCmdFlags.captureTime,
		"capture-time",
		"t",
		30,
		"The time duration in seconds when the capture-data flag is set",
	)

	gatherLogsCmd.Flags().BoolVarP(
		&gatherLogsCmdFlags.yes,
		"yes",
		"y",
		false,
		"Agree to all confirmation prompts.",
	)

	for _, flagName := range []string{
		"capture-data",
		"capture-time",
		"yes",
	} {
		err := gatherLogsCmd.Flags().MarkHidden(flagName)
		if err != nil {
			fmt.Printf("failed configuring cobra: %s\n", err.Error())
			panic("oh no!")
		}
	}

	return gatherLogsCmd
}

var gatherLogsCmdFlags = struct {
	captureData   bool
	captureTime   int
	yes           bool
	localFallback bool
	logLines      uint64
	overwrite     bool
}{}

func runGatherLogsCmd(cmd *cobra.Command, args []string) error {
	// Ensure we can write to any user given log locations
	overridePath := ""
	if len(args) > 0 && args[0] != "" {
		var err error
		overridePath, err = expandOutFilePath(args[0])
		if err != nil {
			return err
		}
		if err = handleOverwriteIfExists(overridePath); err != nil {
			return err
		}
	}

	if gatherLogsCmdFlags.captureData {
		return runCaptureDataLocalCmd(overridePath, gatherLogsCmdFlags.captureTime, gatherLogsCmdFlags.yes)
	}

	if gatherLogsCmdFlags.localFallback {
		return runGatherLogsLocalCmd(overridePath, gatherLogsCmdFlags.logLines)
	}

	return gatherLogsFromServer(overridePath, gatherLogsCmdFlags.logLines)
}

const recoveryMsg = `
Verify and ensure that the deployment-service is running:

	hab svc status
	chef-automate start
	hab svc load chef/deployment-service

Check the deployment-service logs with:

	journalctl -u chef-automate --output=cat | grep deployment-service

Alternatively, gather-logs can be run in local mode with the -l flag:

	chef-automate gather-logs -l
`

// GatherLogs collects system and application logs, and other diagnostic
// information into a support bundle that it transfers to the client
func gatherLogsFromServer(outfileOverride string, logLines uint64) error {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return status.WithRecovery(
			status.Annotate(err, status.GatherLogsError),
			recoveryMsg,
		)
	}

	// Request to create a new bundle and return metadata
	bundleInfo, err := connection.GatherLogs(context.Background(), &api.GatherLogsRequest{
		LogLines: logLines,
	})
	if err != nil {
		return status.WithRecovery(
			status.Wrap(err, status.DeploymentServiceCallError, "Request to gather logs failed"),
			recoveryMsg,
		)
	}

	// Download the bundle via gRPC stream
	stream, err := connection.GatherLogsDownload(
		context.Background(),
		&api.GatherLogsDownloadRequest{
			BundleName: bundleInfo.BundleName,
		},
	)
	if err != nil {
		return status.WithRecovery(
			status.Wrap(err, status.DeploymentServiceCallError, "Request to gather logs failed"),
			recoveryMsg,
		)
	}

	outFilePath := bundleInfo.BundleName
	if outfileOverride != "" {
		outFilePath = outfileOverride
	}

	outFile, err := createOutfile(outFilePath)
	if err != nil {
		return err
	}

	defer func() {
		_ = outFile.Close()
	}()

	logrus.Debugf("Downloading log bundle to: %s ", outFilePath)
	// Write stream to disk
	w := bufio.NewWriter(outFile)
	for {
		resp, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return status.WithRecovery(
				status.Wrap(err, status.DeploymentServiceCallError, "Request to gather logs failed"),
				recoveryMsg,
			)
		}

		data := resp.GetData()
		if data == nil {
			break
		}
		if _, err := w.Write(data); err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
	}

	_ = w.Flush()
	writer.Printf("Log archive written to: %s\n", outFilePath)

	return nil
}

func runCaptureDataLocalCmd(outfileOverride string, captureTime int, IAmSure bool) error {
	// Here we capture CTRL-C to ensure we revert changes made to nginx.conf
	// followed by a HUP to the nginx master process
	c := make(chan os.Signal)
	signal.Notify(c, os.Interrupt, syscall.SIGTERM)
	go func() {
		<-c
		writer.Body("Detected interrupt!")
		tryRevertingAnyNginxConfChanges()
		os.Exit(1)
	}()

	capturePath := os.TempDir()

	writer.Printf(`WARNING: We are about to capture %d seconds of all incoming HTTP data, potentially
causing a significant increase in disk I/O and inode usage of the filesystem for:

  %s
`,
		captureTime, defaultNginxClientBodyPath)

	if !IAmSure {
		yes, err := writer.Confirm(fmt.Sprintf("\nAre you sure you want to continue?"))
		if err != nil {
			return err
		}
		if !yes {
			return status.New(status.InvalidCommandArgsError, "capturing will not be enabled.")
		}
	}

	archiveRoot, err := createTempDir(capturePath, "data-capture")
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			"Creating temporary directory for data-capture failed",
		)
	}

	defer func() {
		_ = os.RemoveAll(archiveRoot)
	}()

	g := gatherlogs.NewGatherer(
		capturePath,
		archiveRoot,
		"chef-automate-local-data-capture",
		binPaths,
		time.Now(),
	)

	writer.Bodyf("Creating data capture bundle directory on %s", capturePath)

	if err := g.CreateBundleDir(); err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			"Creating data capture directory failed",
		)
	}

	writer.Body("Setting \"client_body_in_file_only on;\" in nginx.conf")

	if err := toggleClientBodyInFileOnly(defaultNginxConfPath, true); err != nil {
		tryRevertingAnyNginxConfChanges()
		return status.Wrap(
			err,
			status.FileAccessError,
			"configuring nginx.conf for data capture failed",
		)
	}

	writer.Bodyf("Pausing for %d seconds while nginx writes out the client bodies...", captureTime)

	time.Sleep(time.Duration(captureTime) * time.Second)

	writer.Body("Setting \"client_body_in_file_only off;\" in nginx.conf")

	if err := toggleClientBodyInFileOnly(defaultNginxConfPath, false); err != nil {
		tryRevertingAnyNginxConfChanges()
		return status.Wrap(
			err,
			status.FileAccessError,
			"disabling data-capture in nginx.conf failed",
		)
	}

	waitTimeForDraining := 5

	writer.Body("Waiting a few moments while nginx worker processes drain...")

	time.Sleep(time.Duration(waitTimeForDraining) * time.Second)

	writer.Body("Creating capture bundle file")

	g.AddCopiesFromPath("client-body", defaultNginxClientBodyPath)
	g.ExecuteAll()

	bundleInfo, err := g.CreateBundleFile()
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			"creating capture bundle failed",
		)
	}

	outFilePath := bundleInfo.Name
	if outfileOverride != "" {
		outFilePath = outfileOverride
	}

	// os.Rename is not safe as it will fail if different partition
	out, err := command.CombinedOutput("mv", command.Args(bundleInfo.Path, outFilePath))
	if err != nil {
		return status.Wrap(
			errors.Wrap(err, out),
			status.FileAccessError,
			"renaming data-capture bundle failed",
		)
	}

	writer.Bodyf("\nData capture archive written to: %s\n", outFilePath)

	// clean out the client bodies
	entries, err := filepath.Glob(fmt.Sprintf("%s/*", defaultNginxClientBodyPath))
	if err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			"unable to get directory listing of client body files",
		)
	}
	var errs []error
	for _, f := range entries {
		if err := os.RemoveAll(f); err != nil {
			errs = append(errs, errors.Wrapf(err, "removing client body file(s) failed: %s", f))
			continue
		}
	}
	l := len(errs)
	if l > 0 {
		return status.Wrapf(
			errs[l-1],
			status.FileAccessError,
			"%d error(s) encountered removing files, the last of which was the following",
			l)
	}

	return nil
}

func runGatherLogsLocalCmd(outfileOverride string, logLines uint64) error {
	// TODO: This might not always be the right choice, we should look into
	// assigning this dynamically or allow a config flag. Right now /tmp
	// works on our target distros and this is just an emergency fallback
	stagingDir := "/tmp"
	archiveRoot, err := createTempDir(stagingDir, "log-gathering")
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}

	defer func() {
		_ = os.RemoveAll(archiveRoot)
	}()

	// initialize log gathering configuration
	g := gatherlogs.NewGatherer(
		stagingDir,
		archiveRoot,
		"chef-automate-local-fallback",
		binPaths,
		time.Now(),
	)

	writer.Body("Creating Bundle directory")

	if err := g.CreateBundleDir(); err != nil {
		return status.Annotate(err, status.FileAccessError)
	}

	writer.Body("Gathering...")

	// Chef Automate
	g.AddCommand("chef-automate_release_versions", os.Args[0], "version", "--verbose")
	g.AddCommand("chef-automate_preflight-check", os.Args[0], "preflight-check")
	g.AddCommand("hab_sup_status", "hab", "sup", "status")
	if logLines > 0 {
		logLinesStr := strconv.FormatUint(logLines, 10)
		g.AddCommand("journalctl_chef-automate", "journalctl", "--utc", "-u", "chef-automate", "-u", "hab-sup", "-n", logLinesStr)
	} else {
		g.AddCommand("journalctl_chef-automate", "journalctl", "--utc", "-u", "chef-automate", "-u", "hab-sup")
	}

	// hab version info
	g.AddCommand("hab_version", "hab", "--version")
	g.AddCommand("hab_sup_version", "hab", "sup", "--version")
	g.AddCommand("ulimit_a_hab", "su", "-m", "hab", "-c", "ulimit -a")

	// Habitat applications
	g.AddCommand("find_hab_pkgs", "find", "/hab/pkgs", "-maxdepth", "4", "-mindepth", "4", "-type", "d", "-ls")
	g.AddCopiesFromPath("user.toml", "/hab/user")
	g.AddCopiesFromPath("config", "/hab/svc")
	g.AddCopiesFromPath("logs", "/hab/svc")

	// local status
	g.AddCommand("df_h", "df", "-h")
	g.AddCommand("df_i", "df", "-i")
	g.AddCommand("df_k", "df", "-k")
	g.AddCommand("free_m", "free", "-m")
	g.AddCommand("ps_fauxww", "hab", "pkg", "exec", "core/procps-ng", "ps", "fauxww")
	g.AddCommand("umask", "bash", "-c", "umask")
	g.AddCommand("uname_a", "uname", "-a")
	g.AddCommand("uptime", "uptime")
	g.AddCommand("sysctl_a", "sysctl", "-a")
	g.AddCommand("dmesg", "dmesg", "-T")

	// network
	hostname := g.FetchHostname()
	hostnameFqdn := g.FetchHostname("--fqdn")

	g.AddCommand("dig-fqdn", "dig", hostnameFqdn)
	g.AddCommand("hostname", "hostname")
	g.AddCommand("hostname_--fqdn", "hab", "pkg", "exec", "core/net-tools", "hostname", "--fqdn")
	g.AddCommand("ip_addr_show", "ip", "addr", "show")
	g.AddCommand("ping_-c_2_hostname", "ping", "-c", "2", hostname)
	g.AddCommand("ping_-c_2_fqdn", "ping", "-c", "2", hostnameFqdn)
	g.AddCommand("ss", "ss", "--options", "--numeric", "--tcp", "--all", "--processes")

	// Distro version/release of the underlying operating system.
	// If a distro uses systemd, it provides distro information in
	// /etc/os-release
	// http://0pointer.de/blog/projects/os-release.html
	g.AddCopy("/etc/os-release")
	// Gather the old distro release/version files in addition to what is provided in
	// /etc/os-release in case the content is not the same.
	// Files via mixlib-install's platform detection script.
	// https://github.com/chef/mixlib-install/blob/fe6ecabe54476e0dd6bd7d7553dba1f85337f105/lib/mixlib/install/generator/bourne/scripts/platform_detection.sh
	g.AddCopy("/etc/lsb-release")
	g.AddCopy("/etc/redhat-release")
	g.AddCopy("/etc/debian_version")
	g.AddCopy("/etc/system-release")
	g.AddCopy("/etc/debian-release")
	g.AddCopy("/etc/SuSE-release")
	g.AddCopy("/proc/cpuinfo")
	g.AddCopy("/proc/meminfo")
	g.AddCopy("/etc/resolv.conf")
	g.AddCopy("/etc/hosts")
	g.AddCopy("/proc/cpuinfo")
	g.AddCopy("/proc/sys/crypto/fips_enabled")
	g.AddCopiesFromPath("syslog", "/var/log")
	g.AddCopiesFromPath("messages", "/var/log")

	// automate-gateway metrics
	// FIXME: Move this to using config.
	g.AddURL("automate-gateway_metrics", "https://localhost:2000/metrics")

	g.ExecuteAll()

	writer.Body("Creating Bundle file")

	bundleInfo, err := g.CreateBundleFile()
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}

	outFilePath := bundleInfo.Name
	if outfileOverride != "" {
		outFilePath = outfileOverride
	}

	// os.Rename is not safe as it will fail if different partition
	out, err := exec.Command("mv", bundleInfo.Path, outFilePath).CombinedOutput()
	if err != nil {
		return status.Annotate(errors.Wrap(err, string(out)), status.FileAccessError)
	}

	writer.Printf("Log archive written to: %s\n", outFilePath)
	return nil
}

func createOutfile(path string) (*os.File, error) {
	outFile, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0600)
	if err != nil {
		return nil, status.Annotate(err, status.FileAccessError)
	}

	return outFile, nil
}

func expandOutFilePath(path string) (string, error) {
	path, err := filepath.Abs(path)
	if err != nil {
		return path, status.Annotate(err, status.FileAccessError)
	}

	return path, nil
}

func handleOverwriteIfExists(path string) error {
	_, err := os.Stat(path)
	if os.IsNotExist(err) {
		return nil
	} else if err == nil && !gatherLogsCmdFlags.overwrite {
		msg := fmt.Sprintf("Archive '%s' already exists and overwrite flag not set", path)
		return status.New(status.GatherLogsError, msg)
	} else {
		return err
	}
}

func createTempDir(parent, prefix string) (string, error) {
	tempDir, err := ioutil.TempDir(parent, prefix)
	if err != nil {
		return "", err
	}

	return tempDir, nil
}

func toggleClientBodyInFileOnly(path string, toggledOn bool) error {
	data, err := ioutil.ReadFile(path)
	if err != nil {
		return err
	}

	search, replace := "off", "on"

	if !toggledOn {
		search = "on"
		replace = "off"
	}

	r, err := regexp.Compile(fmt.Sprintf("client_body_in_file_only %s", search))
	if err != nil {
		return err
	}

	entireFileByteSlice := r.ReplaceAll(data, []byte(fmt.Sprintf("client_body_in_file_only %s", replace)))

	reader := bytes.NewReader(entireFileByteSlice)
	if err := fileutils.AtomicWrite(path, reader, fileutils.WithAtomicWriteNoSync(false),
		fileutils.WithAtomicWriteFileMode(0740)); err != nil {
		return status.Wrap(
			err,
			status.FileAccessError,
			"Writing nginx config failed",
		)
	}

	// Send the Nginx master process a SIGHUP to reload config
	pid, err := nginxMasterPid()
	if err != nil {
		return status.Wrap(err, status.FileAccessError, "failed to find nginx master process from PID file")
	}

	nginxMasterProcess, err := os.FindProcess(pid)
	if err != nil {
		return status.Wrap(err, status.UnknownError, "failed to find process from PID")
	}

	writer.Bodyf("Sending SIGHUP to nginx master pid %d to reload config", pid)

	if err := nginxMasterProcess.Signal(syscall.SIGHUP); err != nil {
		return status.Wrap(err, status.UnknownError, "failed to send SIGHUP to nginx master")
	}

	return nil
}

func nginxMasterPid() (int, error) {
	data, err := ioutil.ReadFile(defaultNginxMasterPidFile)
	if err != nil {
		return -1, errors.Wrapf(err, "could not access nginx master pid file (%s)", defaultNginxMasterPidFile)
	}

	pid, err := strconv.ParseInt(string(data), 10, 32)
	if err != nil {
		return -1, errors.Wrapf(err, "failed to parse pid %s", string(data))
	}

	return int(pid), nil
}

func tryRevertingAnyNginxConfChanges() {
	writer.Body("Attempting to revert nginx.conf changes.")

	if err := toggleClientBodyInFileOnly(defaultNginxConfPath, false); err != nil {
		writer.Body("Please manually restart automate and any changes will be reverted")
	} else {
		writer.Body("Successfully reverted all changes")
	}
}

func init() {
	RootCmd.AddCommand(newGatherLogsCmd())
}
