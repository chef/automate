package main

import (
	"context"
	"os"
	"time"

	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-cli/pkg/client/debugclient"
	"github.com/chef/automate/components/automate-cli/pkg/status"
)

var profileCmdFlags = struct {
	endpoint          string
	output            string
	profileName       string
	sampleRate        int
	snapshotDuration  time.Duration
	connectionTimeout time.Duration
}{}

var traceCmdFlags = struct {
	endpoint          string
	output            string
	traceDuration     time.Duration
	connectionTimeout time.Duration
}{}

var logLevelCmdFlags = struct {
	endpoint          string
	connectionTimeout time.Duration
}{}

var versionCmdFlags = struct {
	endpoint          string
	connectionTimeout time.Duration
}{}

func newDebugCmd() *cobra.Command {
	debugCmd := &cobra.Command{
		Use:    "debug",
		Short:  "Enable profiling for services",
		Hidden: true,
	}

	if isDevMode() {
		debugCmd.Hidden = false
	}

	profileCmd := &cobra.Command{
		Use:   "profile service-name",
		RunE:  runProfile,
		Short: "Run the CPU profiler for the service for the given service",
		Args:  cobra.ExactArgs(1),
	}

	traceCmd := &cobra.Command{
		Use:   "trace service-name",
		RunE:  runTrace,
		Short: "Run the execution tracer for the service for the given service",
		Args:  cobra.ExactArgs(1),
	}

	logLevelCmd := &cobra.Command{
		Use:   "set-log-level service-name level",
		RunE:  runSetLogLevel,
		Short: "Sets the log level for the given service",
		Args:  cobra.ExactArgs(2),
	}

	getVersionCmd := &cobra.Command{
		Use:   "get-version service-name",
		RunE:  runGetVersion,
		Short: "Gets the version for the given service",
		Args:  cobra.ExactArgs(1),
	}

	profileCmd.Flags().StringVar(&profileCmdFlags.endpoint, "endpoint", "", "The endpoint the service is listening on")
	profileCmd.Flags().StringVarP(&profileCmdFlags.output, "output", "o", "profile.out", "The file to which the profile snapshot will be written")
	profileCmd.Flags().StringVarP(&profileCmdFlags.profileName, "profile-name", "p", "", "The profile to run")
	profileCmd.Flags().IntVarP(&profileCmdFlags.sampleRate, "rate", "r", 0, "The sample rate")
	profileCmd.Flags().DurationVar(&profileCmdFlags.snapshotDuration, "duration", 0, "The snapshot duration")
	profileCmd.Flags().DurationVar(&profileCmdFlags.connectionTimeout, "connection-timeout", 0, "Most time to wait to connect to service")

	traceCmd.Flags().StringVar(&traceCmdFlags.endpoint, "endpoint", "", "The endpoint the service is listening on")
	traceCmd.Flags().StringVarP(&traceCmdFlags.output, "output", "o", "trace.out", "The file to which the trace will be written")
	traceCmd.Flags().DurationVar(&traceCmdFlags.traceDuration, "duration", 0, "The snapshot duration")
	traceCmd.Flags().DurationVar(&traceCmdFlags.connectionTimeout, "connection-timeout", 0, "Most time to wait to connect to service")

	logLevelCmd.Flags().StringVar(&logLevelCmdFlags.endpoint, "endpoint", "", "The endpoint the service is listening on")
	logLevelCmd.Flags().DurationVar(&logLevelCmdFlags.connectionTimeout, "connection-timeout", 0, "Most time to wait to connect to service")

	versionCmd.Flags().StringVar(&versionCmdFlags.endpoint, "endpoint", "", "The endpoint the service is listening on")
	versionCmd.Flags().DurationVar(&versionCmdFlags.connectionTimeout, "connection-timeout", 0, "Most time to wait to connect to service")

	debugCmd.AddCommand(profileCmd)
	debugCmd.AddCommand(traceCmd)
	debugCmd.AddCommand(logLevelCmd)
	debugCmd.AddCommand(getVersionCmd)

	return debugCmd
}

func runProfile(cmd *cobra.Command, args []string) error {
	f, err := os.Create(profileCmdFlags.output)
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}
	defer func() {
		_ = f.Close()
	}()

	err = debugclient.Profile(context.Background(), debugclient.ProfileRequest{
		ConnectionInfo: debugclient.ConnectionInfo{
			ServiceName:       args[0],
			Endpoint:          profileCmdFlags.endpoint,
			ConnectionTimeout: profileCmdFlags.connectionTimeout,
		},
		ProfileName: profileCmdFlags.profileName,
		Duration:    profileCmdFlags.snapshotDuration,
		SampleRate:  profileCmdFlags.sampleRate,
	}, f)
	if err != nil {
		return status.Annotate(err, status.ProfileError)
	}

	return f.Close()
}

func runTrace(cmd *cobra.Command, args []string) error {
	f, err := os.Create(traceCmdFlags.output)
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}
	defer func() {
		_ = f.Close()
	}()

	err = debugclient.Trace(context.Background(), debugclient.TraceRequest{
		ConnectionInfo: debugclient.ConnectionInfo{
			ServiceName:       args[0],
			Endpoint:          traceCmdFlags.endpoint,
			ConnectionTimeout: traceCmdFlags.connectionTimeout,
		},
		Duration: traceCmdFlags.traceDuration,
	}, f)
	if err != nil {
		return status.Annotate(err, status.TraceError)
	}

	return f.Close()
}

func runSetLogLevel(cmd *cobra.Command, args []string) error {
	return debugclient.SetLogLevel(context.Background(), debugclient.ConnectionInfo{
		ServiceName:       args[0],
		Endpoint:          logLevelCmdFlags.endpoint,
		ConnectionTimeout: logLevelCmdFlags.connectionTimeout,
	}, args[1])
}

var devWarningMessage = `
The server %q did not return version information. To use the debug
version command on this service, ensure that you are setting the Version
and GitRef at link time in your Habitat plan:

    GIT_SHA=$(git rev-parse HEAD)
    GO_LDFLAGS=" -X ${scaffolding_go_base_path}/automate/lib/version.Version=${pkg_release}"
    GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/automate/lib/version.GitSHA=${GIT_SHA}"
    GO_LDFLAGS="${GO_LDFLAGS} -X ${scaffolding_go_base_path}/automate/lib/version.BuildTime=${pkg_release}"


Or, if your service needs more customized version information add a
WithVersionInfo option to the service's secureconn factory:

    server.connFactory = secureconn.NewFactory(*certs, secureconn.WithVersionInfo(
        version.Version,
        version.SHA,
    ))
`

func runGetVersion(cmd *cobra.Command, args []string) error {
	serviceName := args[0]
	resp, err := debugclient.GetVersion(context.Background(), debugclient.ConnectionInfo{
		ServiceName:       serviceName,
		Endpoint:          versionCmdFlags.endpoint,
		ConnectionTimeout: versionCmdFlags.connectionTimeout,
	})
	if err != nil {
		return status.Annotate(err, status.DeploymentServiceCallError)
	}

	if looksWrong(resp.Version) && looksWrong(resp.GitRef) && isDevMode() {
		writer.Warnf(devWarningMessage, serviceName)
	}

	writer.Printf("   Version: %s\n   Git Ref: %s\nGo Version: %s\n", resp.Version, resp.GitRef, resp.GoVersion)
	return nil
}

func looksWrong(s string) bool {
	return s == "" || s == "unknown" || s == "noversion"
}

func init() {
	RootCmd.AddCommand(newDebugCmd())
}
