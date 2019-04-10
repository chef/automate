package main

import (
	"os"
	"os/exec"
	"syscall"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/spf13/cobra"
)

var startCommand = &cobra.Command{
	Use:   "start",
	Short: "Start Chef Automate",
	RunE:  runStartCmd,
}

func runStartCmd(cmd *cobra.Command, args []string) error {
	writer.Title("Starting Chef Automate")
	if isDevMode() {
		if err := exec.Command("hab", "sup", "status").Run(); err == nil {
			return nil
		}

		if err := os.MkdirAll("/hab/sup/default", 0755); err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
		writer.Title("Launching the Habitat Supervisor in the background...")
		out, err := os.Create("/hab/sup/default/sup.log")
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
		startSupCmd := exec.Command("hab", "sup", "run")
		startSupCmd.Env = os.Environ()
		startSupCmd.Env = append(startSupCmd.Env, "DS_DEV=true", "CHEF_AUTOMATE_SKIP_SYSTEMD=true")
		startSupCmd.Stdout = out
		startSupCmd.Stderr = out
		startSupCmd.SysProcAttr = &syscall.SysProcAttr{
			Setpgid: true,
		}
		if err := startSupCmd.Start(); err != nil {
			return status.Annotate(err, status.HabCommandError)
		}
	} else {
		systemctlCmd := exec.Command("systemctl", "start", "chef-automate.service")
		systemctlCmd.Stdout = os.Stdout
		systemctlCmd.Stderr = os.Stderr
		if err := systemctlCmd.Run(); err != nil {
			return status.Annotate(err, status.ServiceStartError)
		}
	}

	return nil
}

func init() {
	RootCmd.AddCommand(startCommand)
}
