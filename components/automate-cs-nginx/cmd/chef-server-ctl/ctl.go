// chef-server-ctl: A minimal wrapper for chef-server-ctl
//
// This command provides users with a subset of the commands available
// in the upstream chef-server-ctl command. Not all of the upstream
// commands are appropriate in the context of Chef Automate.  Further,
// in some cases we need to call the chef-server-ctl command with
// alternate configuration.
//
package main

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/platform/secrets"
)

// TODO(ssd) 2018-08-09: Mad about something that seems hard-coded?
//
// It probably lives in one of the following constants or the
// build-time variables below. These values are passed to the
// underlying chef-server-ctl and knife commands via environment
// variables and options.
const (
	knifeConfigFile     = "/hab/svc/automate-cs-nginx/config/knife_superuser.rb"
	erchefReindexScript = "hab pkg exec chef/oc_erchef reindex-opc-organization"
	erchefDBURI         = "postgresql://automate@127.0.0.1:5432/automate-cs-oc-erchef?sslmode=verify-ca&sslcert=/hab/svc/automate-postgresql/config/server.crt&sslkey=/hab/svc/automate-postgresql/config/server.key&sslrootcert=/hab/svc/automate-postgresql/config/root.crt"  // nolint: lll
	bifrostDBURI        = "postgresql://automate@127.0.0.1:5432/automate-cs-oc-bifrost?sslmode=verify-ca&sslcert=/hab/svc/automate-postgresql/config/server.crt&sslkey=/hab/svc/automate-postgresql/config/server.key&sslrootcert=/hab/svc/automate-postgresql/config/root.crt" // nolint: lll
	bifrostURL          = "https://127.0.0.1:10202"
	lbURL               = "https://127.0.0.1:10200"
	tlsCrt              = "/hab/svc/automate-cs-nginx/config/service.crt"
	tlsKey              = "/hab/svc/automate-cs-nginx/config/service.key"
	tlsCA               = "/hab/svc/automate-cs-nginx/config/root_ca.crt"
)

// These paths are injected at BUILD time based on our dependencies to
// save us a few calls to hab and to try to keep Habitat-like
// dependency isolation.
var (
	Version           = "UNKNOWN"
	RubyPath          = "UNKNOWN"
	BundlePath        = "UNKNOWN"
	KnifePath         = "UNKNOWN"
	ChefServerCtlPath = "UNKNOWN"
)

// A subCommand is a chef-server-ctl command.
type subCommand interface {
	// Run executes the subcommand
	Run(args []string) error
	// Hiddent returns true if the command should be excluded from
	// the default help output.
	Hidden() bool
	// Help summary that is displayed by chef-server-ctl help
	HelpLine() string
}

// unsupported subcommands are not supported in the Automate 2
// integrated Chef server.
type unsupported struct {
	name        string
	alternative string
}

const unsupportedNoAltFmt = "The command %q is not supported by the Chef Automate Chef server.\n"
const unsupportedWithAltFmt = "The command %q is not supported by the Chef Automate Chef server. Instead, try\n    %s\n"

func (c unsupported) Run([]string) error {
	if c.alternative == "" {
		fmt.Printf(unsupportedNoAltFmt, c.name)
	} else {
		fmt.Printf(unsupportedWithAltFmt, c.name, c.alternative)
	}
	return nil
}

func (c unsupported) Hidden() bool     { return true }
func (c unsupported) HelpLine() string { return "" }

// wrapKnife commands replace the wrap-knife-opc commands in the
// upstream chef-server-ctl. While we could pass through to the
// underlying chef-server-ctl, it is straightforward to call our knife
// wrapper directly.
type wrapKnife struct {
	cmdVerb  string
	cmdNoun  string
	helpText string
}

func (c wrapKnife) Run(args []string) error {
	args = append([]string{"opc", c.cmdNoun, c.cmdVerb}, args...)
	cmd := exec.Command(KnifePath, args...)
	cmd.Env = os.Environ()
	cmd.Stdout = os.Stdout
	cmd.Stdin = os.Stdin
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func (c wrapKnife) Hidden() bool     { return false }
func (c wrapKnife) HelpLine() string { return c.helpText }

// native subcommands are subcommands that we've reimplemented
// completely in Go. We should keep these to a minimum.
type native struct {
	runFunc  func([]string) error
	helpText string
}

func (c native) Run(args []string) error {
	return c.runFunc(args)
}

func (c native) Hidden() bool     { return false }
func (c native) HelpLine() string { return c.helpText }

//passthrough subcommands are commands where we call the underlying chef-server-ctl command.
type passthrough struct {
	name     string
	helpText string
}

func (c passthrough) Run(args []string) error {
	// The wrapper in core/chef-server-ctl sets up an environment
	// that assumes the chef/chef-server-ctl service is running.
	// Since we don't want to deploy that service we can't `hab
	// pkg exec` that helper and rather have to call the binstub
	// directly.
	//
	// The binstub we are going to call has an interpreter line
	// that uses `/usr/bin/env ruby`. We don't necessarily have
	// ruby in our path because if we are called from a binlink
	// habitat doesn't set our path.  Thus, we need to inject the
	// path to both ruby and bundler.
	existingPath := os.Getenv("PATH")
	var pathEnv string
	if existingPath != "" {
		pathEnv = fmt.Sprintf("PATH=%s:%s/bin:%s/bin", existingPath, RubyPath, BundlePath)
	} else {
		pathEnv = fmt.Sprintf("PATH=%s/bin:%s/bin", RubyPath, BundlePath)
	}

	// Get bifrostSuperuserID from the secrets store
	secretsStore := secrets.NewDiskStoreReader(secrets.DefaultDiskStoreDataDir)
	var bifrostSuperuserID string
	bifrostSecData, err := secretsStore.GetSecret(secrets.BifrostSuperuserIDName)
	if err != nil {
		// We are silently dropping this error for now so that
		// you can run commands that don't require the
		// bifrostSuperuserID, in case bifrost hasn't come up.
		logrus.WithError(err).Debugf("failed to get the bifrost superuser id")
		bifrostSuperuserID = ""
	} else {
		bifrostSuperuserID = string(bifrostSecData)
	}

	// chef-server-ctl has been modified to take all necessary
	// config via environment variables. All CSC_ variables are
	// chef-server-ctl specific configuration.
	env := []string{
		pathEnv,
		fmt.Sprintf("CSC_LB_URL=%s", lbURL),
		fmt.Sprintf("CSC_BIFROST_SUPERUSER_ID=%s", bifrostSuperuserID),
		fmt.Sprintf("CSC_BIFROST_URL=%s", bifrostURL),
		fmt.Sprintf("CSC_BIFROST_DB_URI=%s", bifrostDBURI),
		fmt.Sprintf("CSC_ERCHEF_DB_URI=%s", erchefDBURI),
		fmt.Sprintf("CSC_ERCHEF_REINDEX_SCRIPT=%s", erchefReindexScript),
		fmt.Sprintf("CSC_KNIFE_CONFIG_FILE=%s", knifeConfigFile),
		fmt.Sprintf("CSC_TLS_KEY=%s", tlsKey),
		fmt.Sprintf("CSC_TLS_CRT=%s", tlsCrt),
		fmt.Sprintf("CSC_TLS_CA=%s", tlsCA),
		fmt.Sprintf("CSC_KNIFE_BIN=%s exec %s",
			filepath.Join(BundlePath, "bin", "bundle"),
			filepath.Join(ChefServerCtlPath, "chef", "bin", "knife")),
		"CSC_FIPS_ENABLED=false",
		"CSC_HABITAT_MODE=true",
		fmt.Sprintf("BUNDLE_GEMFILE=%s", filepath.Join(ChefServerCtlPath, "omnibus-ctl", "Gemfile")),
	}

	// remove possibly bad ruby Env settings that could break us.
	// ignore errors because the underlying syscall only fails if:
	// * EINVAL: name is NULL, points to a string of length 0, or contains an '=' character.
	// * ENOMEM
	os.Unsetenv("RUBYOPT")
	os.Unsetenv("GEM_HOME")
	os.Unsetenv("GEM_PATH")
	os.Unsetenv("BUNDLE_GEMFILE")
	os.Unsetenv("BUNDLE_PATH")

	bundlerCmd := filepath.Join(BundlePath, "bin", "bundle")
	chefServerCtlCmd := filepath.Join(ChefServerCtlPath, "omnibus-ctl", "binstubs", "chef-server-ctl")
	cmd := exec.Command(bundlerCmd, append([]string{"exec", chefServerCtlCmd, c.name}, args...)...)
	cmd.Env = append(os.Environ(), env...)
	cmd.Stdout = os.Stdout
	cmd.Stdin = os.Stdin
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func (c passthrough) Hidden() bool     { return false }
func (c passthrough) HelpLine() string { return c.helpText }

const (
	topLevelKey    = "__TOP_LEVEL__"
	unsupportedKey = "__UNSUPPORTED__"
)

// SubCommands is a map of subcommand name to the SubCommand
// interfaces that implement them.
//
// nolint: govet
var subCommands = map[string]map[string]subCommand{
	"Organization and User Management": {
		"org-create":      wrapKnife{"create", "org", "Create an organization in the Chef server."},
		"org-delete":      wrapKnife{"delete", "org", "Delete an organization in the Chef server."},
		"org-list":        wrapKnife{"list", "org", "List all organizations in the Chef server."},
		"org-show":        wrapKnife{"show", "org", "Show an organization in the Chef server."},
		"org-user-add":    wrapKnife{"add", "org user", "Associate a user with an organization."},
		"org-user-remove": wrapKnife{"remove", "org user", "Dissociate a user with an organization."},
		"user-create":     wrapKnife{"create", "user", "Create a user in the Chef server."},
		"user-delete":     wrapKnife{"delete", "user", "Delete a user in the Chef server."},
		"user-edit":       wrapKnife{"edit", "user", "Edit a user in the Chef server."},
		"user-list":       wrapKnife{"list", "user", "List all users in the Chef server."},
		"user-show":       wrapKnife{"show", "user", "Show a user in the Chef server."},
		"password":        passthrough{"password", "Reset a user's password."},
	},
	"Debugging and Maintenance": {
		"cleanup-bifrost": passthrough{"cleanup-bifrost", "Cleanup orphaned authorization objects."},
		"filtered-dump":   passthrough{"filtered-dump", "Generate a filtered dump of all indexable Chef Objects."},
		"reindex":         passthrough{"reindex", "Destroy and recreate the Chef server search index."},
		"test": native{func(args []string) error {
			env := []string{
				fmt.Sprintf("SUPERUSER_KEY=/hab/svc/automate-cs-oc-erchef/data/pivotal.pem"),
				fmt.Sprintf("WEBUI_KEY=/hab/svc/automate-cs-oc-erchef/data/webui_priv.pem"),
				fmt.Sprintf("BUNDLE_GEMFILE=%s", filepath.Join(ChefServerCtlPath, "oc-chef-pedant/Gemfile")),
			}

			bundleCmd := filepath.Join(BundlePath, "bin/bundle")
			cmdArgs := []string{
				"exec", filepath.Join(ChefServerCtlPath, "oc-chef-pedant/bin/oc-chef-pedant"),
				"--log-file", "/dev/null",
				"-c", "/hab/svc/automate-cs-oc-erchef/config/pedant_config.rb",
			}

			// If no arguments are passed default to smoke tests
			if len(args) < 1 {
				cmdArgs = append(cmdArgs, []string{"--focus", "smoke"}...)
			}

			cmd := exec.Command(bundleCmd, append(cmdArgs, args...)...)
			cmd.Env = append(os.Environ(), env...)
			cmd.Stdout = os.Stdout
			cmd.Stdin = os.Stdin
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}, "Run the Chef server test suite"},
	},
	"Key Rotation": {
		"add-client-key":    passthrough{"add-client-key", "Add a client key."},
		"add-user-key":      passthrough{"add-user-key", "Add a user key."},
		"delete-client-key": passthrough{"delete-client-key", "Delete a client key."},
		"delete-user-key":   passthrough{"delete-user-key", "Delete a user key."},
		"list-client-keys":  passthrough{"list-client-keys", "List keys for a client."},
		"list-user-keys":    passthrough{"list-user-keys", "List keys for a user."},
	},
	"Server Administrators": {
		"grant-server-admin-permissions":  passthrough{"grant-server-admin-permissions", "Make a user a server admin."},
		"list-server-admins":              passthrough{"list-server-admins", "List server admins."},
		"remove-server-admin-permissions": passthrough{"remove-server-admin-permissions", "Remove a server admin."},
	},
	topLevelKey: {
		// TODO(ssd) 2018-08-09: The version commands are now
		// inconsistent across the three different install types we
		// have.
		"version": native{func(args []string) error {
			fmt.Printf("Chef Automate Chef server %s\n", Version)
			fmt.Printf("For more information run \n   chef-automate version.\n")
			return nil
		}, "Display the Chef server version."},
	},
	unsupportedKey: {
		// Unsupported Commands:
		// - Secrets Management Command
		// - High Availability
		// - Service Management
		// - Most "general" commands
		//
		// NOTE(ssd) 2018-08-07: We don't really know what is
		// happening with partybus/mover yet but let's avoid needing "rebuild-migration-state" if we can.
		"oc-id-show-app":              unsupported{name: "oc-id-show-app"},
		"remove-secret":               unsupported{name: "remove-secret"},
		"require-credential-rotation": unsupported{name: "require-credential-rotation"},
		"rotate-all-credentials":      unsupported{name: "rotate-all-credentials"},
		"rotate-credentials":          unsupported{name: "rotate-credentials"},
		"rotate-shared-secrets":       unsupported{name: "rotate-shared-secrets"},
		"set-actions-password":        unsupported{name: "set-actions-password"},
		"set-db-superuser-password":   unsupported{name: "set-db-superuser-password"},
		"set-secret":                  unsupported{name: "set-secret"},
		"show-secret":                 unsupported{name: "show-secret"},
		"show-service-credentials":    unsupported{name: "show-service-credentials"},
		"backup-recover":              unsupported{name: "backup-recover"},
		"ha-status":                   unsupported{name: "ha-status"},
		"master-recover":              unsupported{name: "master-recover"},
		"graceful-kill":               unsupported{name: "graceful-kill"},
		"hup":                         unsupported{name: "hup"},
		"int":                         unsupported{name: "int"},
		"kill":                        unsupported{name: "kill"},
		"once":                        unsupported{name: "once"},
		"restart":                     unsupported{name: "restart", alternative: "chef-automate restart-services"},
		"service-list":                unsupported{name: "service-list", alternative: "chef-automate service-versions"},
		"start":                       unsupported{name: "start", alternative: "systemctl start chef-automate.service"},
		"stop":                        unsupported{name: "stop", alternative: "chef-automate stop"},
		"status":                      unsupported{name: "status", alternative: "chef-automate status"},
		"tail":                        unsupported{name: "tail", alternative: "journalctl -f -u chef-automate"},
		"term":                        unsupported{name: "term"},
		"usr1":                        unsupported{name: "usr1"},
		"usr2":                        unsupported{name: "usr2"},
		"backup":                      unsupported{name: "backup", alternative: "chef-automate backup create"},
		"cleanse":                     unsupported{name: "cleanse", alternative: "chef-automate unistall"},
		"cleanup":                     unsupported{name: "cleanup"},
		"rebuild-migration-state":     unsupported{name: "rebuild-migration-state"},
		"restore":                     unsupported{name: "restore", alternative: "chef-automate backup restore"},
		"show-config":                 unsupported{name: "restore", alternative: "chef-automate config show"},
		"gather-logs":                 unsupported{name: "gather-logs", alternative: "chef-automate gather-logs"},
		"install":                     unsupported{name: "install"},
		"psql":                        unsupported{name: "psql", alternative: "chef-automate dev psql"},
		"uninstall":                   unsupported{name: "uninstall", alternative: "chef-automate uninstall"},
		"upgrade":                     unsupported{name: "upgrade", alternative: "chef-automate upgrade"},
	},
}

func main() {
	if len(os.Args) < 2 {
		printHelp()
		os.Exit(1)
	}

	subcommand := os.Args[1]

	if isHelpCommand(subcommand) {
		printHelp()
		os.Exit(0)
	}

	c, ok := findSubcommand(subcommand)
	if !ok {
		fmt.Fprintf(os.Stderr, "Unknown command %q. See %s --help for available commands\n", subcommand, os.Args[0]) // nolint errcheck
		os.Exit(1)
	}

	err := c.Run(os.Args[2:])
	if err != nil {
		os.Exit(1)
	}
}

func findSubcommand(commandName string) (subCommand, bool) {
	for _, categorySubCommands := range subCommands {
		for name, c := range categorySubCommands {
			if name == commandName {
				return c, true
			}
		}
	}

	return nil, false
}

// Note this is iterates over all of the commands a few times, but
// since there aren't many I don't think it will be much of a problem.
func printHelp() {
	// Calculate longest lines of all names across all categories
	maxLen := 0
	for _, categorySubCommands := range subCommands {
		for name := range categorySubCommands {
			if len(name) > maxLen {
				maxLen = len(name)
			}
		}
	}

	helpLineFmt := fmt.Sprintf("  %%-%ds    %%s\n", maxLen)
	buf := new(strings.Builder)
	fmt.Fprintf(buf, "Chef Automate chef-server-ctl %s\n\n", Version) // nolint errcheck
	for category, categoryCommands := range subCommands {
		switch category {
		case topLevelKey:
			continue
		case unsupportedKey:
			continue
		default:
			fmt.Fprintf(buf, "%s\n", category) // nolint errcheck
			printHelpForCategory(buf, helpLineFmt, categoryCommands)
			fmt.Fprintf(buf, "\n") // nolint errcheck
		}
	}

	// Print top level commands at the end
	printHelpForCategory(buf, helpLineFmt, subCommands[topLevelKey])

	// Add the help "command"
	fmt.Fprintf(buf, helpLineFmt, "help", "Print this help message.") // nolint errcheck
	fmt.Print(buf.String())
}

func printHelpForCategory(buf io.Writer, helpLineFmt string, commands map[string]subCommand) {
	commandNames := make([]string, 0, len(commands))
	for name := range commands {
		commandNames = append(commandNames, name)
	}

	sort.Strings(commandNames)
	for _, name := range commandNames {
		c := commands[name]
		if !c.Hidden() {
			fmt.Fprintf(buf, helpLineFmt, name, c.HelpLine()) // nolint errcheck
		}
	}
}

func isHelpCommand(cmd string) bool {
	return cmd == "-h" || cmd == "--help" || cmd == "help"
}
