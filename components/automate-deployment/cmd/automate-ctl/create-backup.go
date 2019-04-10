// Copyright © 2018 Chef Software

package main

import (
	"crypto/tls"
	"fmt"
	"net/http"
	"os"

	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"

	"github.com/spf13/cobra"
)

var createBackupNoWait = false

func init() {
	RootCmd.AddCommand(automateCtlCreateBackupCmd())
}

// automateCtlCreateBackupCmd represents the automateCtl/create-backup command
func automateCtlCreateBackupCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "create-backup <BACKUP_NAME>",
		Short: "test double of `automate-ctl create-backup`",
		Long: `automate-ctl create-backup will exit 0 by default.
To test failure cases set the FAILURE environment to 400:

FAILURE=400 automate-ctl create-backup

`,
		Run: automateCtlCreateBackup,
	}

	cmd.Flags().BoolVar(&createBackupNoWait,
		"no-wait", false, "don't wait for the es snapshot to complete")
	return cmd
}

func automateCtlCreateBackup(cmd *cobra.Command, args []string) {
	if !createBackupNoWait {
		// in upgrade-from-v1 we always set --no-wait and then poll for es snapshot
		// status via es API
		fmt.Println("Expected --no-wait option to be set, failing")
		os.Exit(10)
	}
	if len(args) != 1 {
		// in upgrade-from-v1 we always set the backup name so we can correctly
		// query for the es snapshot status
		fmt.Println("Expected BACKUP_NAME argument, failing")
		os.Exit(10)
	}

	// the a1 stub server only supports localhost:443
	tlsConfig := &tls.Config{InsecureSkipVerify: true}
	tr := &http.Transport{TLSClientConfig: tlsConfig}
	client := &http.Client{Transport: tr}
	_, err := client.Get("https://localhost/th-createbackup")
	if err != nil {
		fmt.Printf("automate-ctl create-backup TEST HARNESS ERROR %s\n", err.Error())
		os.Exit(10)
	}
	if a1upgrade.Failure(a1upgrade.A1BackupFail) {
		fmt.Println("automate-ctl create-backup FAILURE (simulated)")
		os.Exit(1)
	}
	fmt.Println("automate-ctl create-backup SUCCESS (simulated)")
	os.Exit(0)

}
