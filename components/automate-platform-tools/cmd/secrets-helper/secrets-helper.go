// secrets-helper: a chef_secrets-compatible secrets helper. This is
// basically a reimplementation of veil-env-helper, veil-ingest-secret,
// and veil-read-secret.
//
// This is used to share secrets between components that require
// secrets other than our TLS certificates. For example, erchef and
// bifrost need to share a superuser_id.
//
// The current implementation uses a files on disk for
// storage.
//
// KNOWN ISSUES
//
// - generate and insert are not safe for concurrent access to the
//   same key. A basic check is in place to allow "idempotent"-ish
//   operations for a single service, but if multiple services are
//   generating the same key the last writer wins. However, we ARE
//   concurrency safe for /different/ keys since those are stored in
//   different files on disk.
//
package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/lib/secrets"
)

//
// Option structs
//
type globalOptions struct {
	// Enables debug logging output
	debug bool
	// Path to the shared data directory
	dataDir string
	// Username of the user who should own the data directory.
	owner string
}

type execOptions struct {
	requiredSecrets []string
	optionalSecrets []string
}

type insertOptions struct {
	ifNotExists bool
}

type generateOptions struct {
	ifNotExists bool
}

var globalOpts = globalOptions{}
var execOpts = execOptions{}
var insertOpts = insertOptions{}
var generateOpts = generateOptions{}

// These structs implement flags.Value allowing us to generate options
// to pass to cobra with AddGoFlag. As far as I can see this is the
// only way in cobra to allow multiple invocations fo the same flag to
// build up an array value.
type requiredSecret struct{}
type optionalSecret struct{}

// TODO(ssd) 2018-08-20: Should String() return something sensible?
func (i requiredSecret) String() string { return "" }
func (i requiredSecret) Set(value string) error { // nolint: unparam
	execOpts.requiredSecrets = append(execOpts.requiredSecrets, value)
	return nil
}

func (i optionalSecret) String() string { return "" }
func (i optionalSecret) Set(value string) error { // nolint: unparam
	execOpts.optionalSecrets = append(execOpts.optionalSecrets, value)
	return nil
}

func newCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "secrets-helper COMMAND",
		Short: "Automate Secrets Helper",
		Long:  "A chef_secrets compatible tool for generating and sharing internal secrets",
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			if globalOpts.debug {
				logrus.SetLevel(logrus.DebugLevel)
			} else {
				logrus.SetLevel(logrus.WarnLevel)
			}
			return nil
		},
	}

	cmd.PersistentFlags().BoolVarP(
		&globalOpts.debug,
		"debug",
		"d",
		false,
		"Enabled debug output")
	cmd.PersistentFlags().StringVar(
		&globalOpts.dataDir,
		"data-dir",
		secrets.DefaultDiskStoreDataDir,
		"Path to the shared data directory")
	cmd.PersistentFlags().StringVar(
		&globalOpts.owner,
		"owner",
		secrets.DefaultDiskStoreDataOwner,
		"Owner of the shared data directory")

	initCmd := &cobra.Command{
		Use:   "init",
		Short: "Initialize the secrets store",
		Long:  "Initialize any data required to use the secrets store",
		Run:   initialize,
	}

	showCmd := &cobra.Command{
		Use:   "show NAME",
		Short: "Print the named secret",
		Long:  "Print the named secret to standard output",
		Run:   show,
		Args:  cobra.ExactArgs(1),
	}

	insertCmd := &cobra.Command{
		Use:   "insert NAME",
		Short: "Insert secret data into the secret store",
		Long:  "Insert secret data into the secret store under the key NAME. Secret data is read from standard input",
		Run:   insert,
		Args:  cobra.ExactArgs(1),
	}

	generateCmd := &cobra.Command{
		Use:   "generate NAME LEN",
		Short: "Generate a new secrets",
		Long:  "Generate a secret of length LEN. Secrets will be in hexadecimal, the length is the desired ASCII length.",
		Run:   generate,
		Args:  cobra.ExactArgs(2),
	}

	execCmd := &cobra.Command{
		Use:   "exec [OPTIONS] -- COMMAND",
		Short: "Exec a command with the given secrets",
		Long:  "Exec the given command with chef_secrets-formatted secrets data in its environment",
		Run:   execCommand,
		Args:  cobra.MinimumNArgs(1),
	}

	execCmd.PersistentFlags().AddGoFlag(&flag.Flag{
		Name:  "secret",
		Usage: "Required secret to pass to service",
		Value: requiredSecret{},
	})

	execCmd.PersistentFlags().AddGoFlag(&flag.Flag{
		Name:  "optional-secret",
		Usage: "Optional secret to pass to service",
		Value: optionalSecret{},
	})

	insertCmd.PersistentFlags().BoolVar(
		&insertOpts.ifNotExists, "if-not-exists", false, "Exit without an error if value exists")
	generateCmd.PersistentFlags().BoolVar(
		&generateOpts.ifNotExists, "if-not-exists", false, "Exit without an error if value exists")

	cmd.AddCommand(initCmd)
	cmd.AddCommand(showCmd)
	cmd.AddCommand(insertCmd)
	cmd.AddCommand(generateCmd)
	cmd.AddCommand(execCmd)

	return cmd
}

func main() {
	err := newCmd().Execute()
	if err != nil {
		// NOTE(ssd) 2018-08-20: We expect subcommands to
		// handle their own errors and exit as appropriate.
		logrus.Fatal(errors.Wrap(err, "unhandled error"))
	}
}

func newSecretsStore() (secrets.SecretStore, error) {
	u, err := user.Lookup(globalOpts.owner)
	if err != nil {
		return nil, errors.Wrap(err, "user lookup")
	}

	uid, err := strconv.Atoi(u.Uid)
	if err != nil {
		return nil, errors.Wrap(err, "converting uid to integer")
	}

	gid, err := strconv.Atoi(u.Gid)
	if err != nil {
		return nil, errors.Wrap(err, "converting gid to integer")
	}

	return secrets.NewDiskStore(globalOpts.dataDir, uid, gid), nil
}

//
// Subcommand funcs
//
// NOTE(ssd) 2018-08-20: These top level functions assume that the
// secret store or underlying functions will be wrapping errors with
// sensible message.
func initialize(_ *cobra.Command, args []string) {
	secretsStore, err := newSecretsStore()
	if err != nil {
		logrus.Fatal(err)
	}

	err = secretsStore.Initialize()
	if err != nil {
		logrus.Fatal(err)
	}
}

func show(_ *cobra.Command, args []string) {
	secretName, err := secrets.SecretNameFromString(args[0])
	if err != nil {
		logrus.Fatal(err)
	}

	secretsStore, err := newSecretsStore()
	if err != nil {
		logrus.Fatal(err)
	}

	content, err := secretsStore.GetSecret(secretName)
	if err != nil {
		logrus.Fatal(err)
	}
	fmt.Printf("%s\n", content)
}

func insert(_ *cobra.Command, args []string) {
	secretName, err := secrets.SecretNameFromString(args[0])
	if err != nil {
		logrus.Fatal(err)
	}

	secretsStore, err := newSecretsStore()
	if err != nil {
		logrus.Fatal(err)
	}

	exists, err := secretsStore.Exists(secretName)
	if err != nil {
		logrus.Fatal(err)
	}

	if exists && insertOpts.ifNotExists {
		logrus.Infof("Secret %q already exists, skipping insertion", secretName)
		os.Exit(0)
	}

	content, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		logrus.Fatal(errors.Wrap(err, "error reading secret data from standard input"))
	}

	err = secretsStore.SetSecret(secretName, content)
	if err != nil {
		logrus.Fatal(err)
	}
}

func generate(_ *cobra.Command, args []string) {
	secretName, err := secrets.SecretNameFromString(args[0])
	if err != nil {
		logrus.Fatal(err)
	}

	secretsStore, err := newSecretsStore()
	if err != nil {
		logrus.Fatal(err)
	}

	exists, err := secretsStore.Exists(secretName)
	if err != nil {
		logrus.Fatal(err)
	}

	if exists && generateOpts.ifNotExists {
		logrus.Infof("Secret %s already exists, skipping generation", secretName)
		os.Exit(0)
	}

	userProvidedLength, err := strconv.Atoi(args[1])
	if err != nil {
		logrus.Fatal(errors.Wrapf(err, "could not convert provided length (%s) to integer", args[1]))
	}

	randomBytes, err := secrets.GenerateRandomBytes(userProvidedLength)
	if err != nil {
		logrus.Fatal(err)
	}

	err = secretsStore.SetSecret(secretName, randomBytes)
	if err != nil {
		logrus.Fatal(err)
	}
}

func execCommand(_ *cobra.Command, args []string) {
	fullExe, err := resolveExe(args[0])
	if err != nil {
		logrus.Fatal(err)
	}

	secretsStore, err := newSecretsStore()
	if err != nil {
		logrus.Fatal(err)
	}

	secretData := make(map[string]map[string]string, 8)
	for _, secretSpec := range execOpts.requiredSecrets {
		secretName, err := secrets.SecretNameFromString(secretSpec)
		if err != nil {
			logrus.Fatal(err)
		}

		content, err := secretsStore.GetSecret(secretName)
		if err != nil {
			logrus.Fatal(err)
		}

		_, ok := secretData[secretName.Group]
		if !ok {
			secretData[secretName.Group] = make(map[string]string)
		}

		secretData[secretName.Group][secretName.Name] = string(content)
	}

	for _, secretSpec := range execOpts.optionalSecrets {
		secretName, err := secrets.SecretNameFromString(secretSpec)
		if err != nil {
			logrus.Fatal(err)
		}

		content, err := secretsStore.GetSecret(secretName)
		if err != nil {
			logrus.Warn(err)
			continue
		}

		_, ok := secretData[secretName.Group]
		if !ok {
			secretData[secretName.Group] = make(map[string]string)
		}

		secretData[secretName.Group][secretName.Name] = string(content)
	}

	secretJson, err := json.Marshal(secretData)
	if err != nil {
		logrus.Fatal(errors.Wrap(err, "failed to marshal secrets data into JSON"))
	}

	// Open a pipe that we will write the secrets into. The called
	// processes can read the secrets from the read-end of the
	// pipe.
	readPipe, writePipe, err := os.Pipe()
	if err != nil {
		logrus.Fatal(errors.Wrap(err, "could not open secrets pipe"))
	}

	_, err = writePipe.Write(secretJson)
	if err != nil {
		logrus.Fatal(errors.Wrap(err, "could not write json data to secrets pipe"))
	}

	// We avoid using `defer` here because we will be exec'ing
	// before this function returns.
	err = writePipe.Close()
	if err != nil {
		logrus.Fatal(errors.Wrap(err, "could not close write-end of secrets pipe"))
	}

	// Clear FD_CLOEXEC flag on the read pipe so that the process
	// we exec below can still open it.
	_, _, errno := syscall.Syscall(syscall.SYS_FCNTL, readPipe.Fd(), syscall.F_SETFD, 0)
	if errno != 0 {
		logrus.Fatal(errors.Wrap(err, "failed to clear CLOEXEC flag on secrets pipe"))
	}

	logrus.WithFields(logrus.Fields{
		"secrets_fd": readPipe.Fd(),
		"full_exe":   fullExe,
		"args":       args,
	}).Debug("Executing command")
	env := append(os.Environ(), fmt.Sprintf("CHEF_SECRETS_FD=%d", readPipe.Fd()))
	err = syscall.Exec(fullExe, args, env)
	if err != nil {
		logrus.Fatal(errors.Wrap(err, "failed to exec command"))
	}
}

// resolveExe finds full path to the given executable, searching the
// PATH if necessary. syscall.Exec uses execve which requires the
// full path to the executable and does not search the path.
func resolveExe(exe string) (string, error) {
	if strings.HasPrefix(exe, "/") {
		return exe, nil
	}

	if strings.HasPrefix(exe, "./") {
		fullExe, err := filepath.Abs(exe)
		if err != nil {
			return "", errors.Wrap(err, "could not resolve relative command path to absolute path")
		}

		return fullExe, nil

	}

	fullExe, err := exec.LookPath(exe)
	if err != nil {
		return "", errors.Wrap(err, "could not find command in PATH")
	}

	return fullExe, nil
}
