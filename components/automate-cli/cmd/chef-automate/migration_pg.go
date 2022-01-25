package main

import (
	"errors"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strconv"
	"syscall"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/user"
	"github.com/spf13/cobra"
)

var migratePgCmdFlags = struct {
	fromVersion      string
	toVersion        string
	oldDatadir       string
	newDatadir       string
	oldBindir        string
	newBindir        string
	oldOptions       string
	newOptions       string
	check            bool
	pgUpgradeBinPath string
}{}

const AUTOMATE_PG_MIGRATE_LOG_DIR = "/src/logs"

func init() {
	migrateCmd.AddCommand(newMigratePgCmd())
	migrateCmd.AddCommand(newRemovePgDatadirCmd())
	RootCmd.AddCommand(migrateCmd)
}

var migrateCmd = &cobra.Command{
	Use:    "migrate COMMAND",
	Short:  "Utilities for Chef Automate development",
	Hidden: true,
}

func newRemovePgDatadirCmd() *cobra.Command {
	var removePgDatadirCmd = &cobra.Command{
		Use:   "cleanup",
		Short: "Chef Automate migrate_pg",
		Long:  "Chef Automate migrate_pg. from one version to another",
		RunE:  runCleanup,
	}
	return removePgDatadirCmd
}

func newMigratePgCmd() *cobra.Command {
	var migratePgCmd = &cobra.Command{
		Use:   "pg",
		Short: "Chef Automate migrate_pg",
		Long:  "Chef Automate migrate_pg. from one version to another",
		RunE:  runMigratePgCmd,
	}
	migratePgCmd.PersistentFlags().StringVar(&migratePgCmdFlags.fromVersion, "from-version", "9.6.21", "from version")
	migratePgCmd.PersistentFlags().StringVar(&migratePgCmdFlags.toVersion, "to-version", "13.4", "to version")
	migratePgCmd.PersistentFlags().StringVar(&migratePgCmdFlags.oldDatadir, "old-datadir", "/hab/svc/automate-postgresql/data/pgdata", "old datadir")
	migratePgCmd.PersistentFlags().StringVar(&migratePgCmdFlags.newDatadir, "new-datadir", "/hab/svc/automate-postgresql/data/pgdata13", "new datadir")
	migratePgCmd.PersistentFlags().StringVar(&migratePgCmdFlags.oldBindir, "old-bindir", "/hab/pkgs/core/postgresql/9.6.21/20211016180117/bin", "old bindir")
	migratePgCmd.PersistentFlags().StringVar(&migratePgCmdFlags.newBindir, "new-bindir", "/hab/pkgs/core/postgresql13/13.4/20210827075515/bin", "new bindir")
	migratePgCmd.PersistentFlags().BoolVar(&migratePgCmdFlags.check, "check", false, "check")
	migratePgCmd.PersistentFlags().StringVar(&migratePgCmdFlags.pgUpgradeBinPath, "pg-upgrade-bin-path", "/hab/pkgs/core/postgresql13/13.4/20210827075515/bin/pg_upgrade", "pg_upgrade bin path")
	return migratePgCmd
}

func runCleanup(cmd *cobra.Command, args []string) error {
	writer.Title("remove pg datadir")
	removePgDatadir()
	writer.Title("Deleting file created by pg_upgrade")
	cleanUp()
	return nil
}

func runMigratePgCmd(cmd *cobra.Command, args []string) error {
	chefAutomateStop()
	removeAndReplacePgdata13()
	executePgdata13ShellScript()
	writer.Title(" migration from: " + migratePgCmdFlags.fromVersion + " to: " + migratePgCmdFlags.toVersion)
	writer.Title("--------------------------------")
	checkUpdateMigration(migratePgCmdFlags.check)
	chefAutomateStart()
	chefAutomateStatus()
	vacuumDb()
	return nil
}

func vacuumDb() {
	args := []string{
		"./analyze_new_cluster.sh",
	}
	c := exec.Command("/bin/sh", args...)
	checkErrorForCommand(c)
}

func cleanUp() {

	c := exec.Command("rm", "-rf", "./analyze_new_cluster.sh", "./delete_old_cluster.sh", "pgmigrate.log")
	checkErrorForCommand(c)
}

func removePgDatadir() {
	writer.Title("remove pg datadir")
	writer.Title("--------------------------------")
	removePgDatadir := exec.Command("rm", "-rf", migratePgCmdFlags.oldDatadir)
	checkErrorForCommand(removePgDatadir)
}

func chefAutomateStop() {
	args := []string{
		"stop",
	}

	stopChefAutomate := exec.Command("chef-automate", args...)
	checkErrorForCommand(stopChefAutomate)
}

func chefAutomateStatus() {
	args := []string{
		"status",
	}

	statusChefAutomate := exec.Command("chef-automate", args...)
	checkErrorForCommand(statusChefAutomate)
}

func removeAndReplacePgdata13() {
	argsToRemove := []string{
		"-rf",
		"/hab/svc/automate-postgresql/data/pgdata13",
	}

	removePgDatadir := exec.Command("rm", argsToRemove...)
	checkErrorForCommand(removePgDatadir)

}

func chefAutomateStart() {
	args := []string{
		"start",
	}
	chefAutomateStart := exec.Command("chef-automate", args...)
	checkErrorForCommand(chefAutomateStart)
}

func executePgdata13ShellScript() {
	args := []string{
		"./pgdata13.sh",
	}
	c := exec.Command("/bin/sh", args...)
	c.SysProcAttr = &syscall.SysProcAttr{}
	uid, gid, err := lookupUser("hab")
	if err != nil {
		fmt.Printf("failed fetching hab uid and gid: %s\n", err.Error())
		return
	}
	c.SysProcAttr.Credential = &syscall.Credential{Uid: uint32(uid), Gid: uint32(gid)}
	checkErrorForCommand(c)
}

func checkUpdateMigration(check bool) {

	writer.Title("Checking for pg_upgrade")
	args := []string{
		"--old-datadir=/hab/svc/automate-postgresql/data/pgdata",
		"--new-datadir=/hab/svc/automate-postgresql/data/pgdata13",
		"--old-bindir=/hab/pkgs/core/postgresql/9.6.21/20211016180117/bin",
		"--new-bindir=/hab/pkgs/core/postgresql13/13.4/20220120060519/bin",
		"--check",
		"-U",
		"automate",
	}

	if !check {
		strSlice := RemoveIndex(args, 4)
		args = strSlice
	}
	err := executeAutomateCommandAsync(
		"/hab/pkgs/core/postgresql13/13.4/20220120060519/bin/pg_upgrade",
		args,
		"",
		AUTOMATE_PG_MIGRATE_LOG_DIR+"/pgmigrate.log")
	if err != nil {
		fmt.Println(err)
	}
}

func checkErrorForCommand(executable *exec.Cmd) {
	out, err := executable.Output()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(string(out))
}

func executeAutomateCommandAsync(command string, args []string, helpDocs string, logFilePath string) error {
	if len(command) < 1 {
		return errors.New("Invalid or empty command")
	}
	if _, err := os.Stat(AUTOMATE_PG_MIGRATE_LOG_DIR); !errors.Is(err, nil) {
		err = os.Mkdir(AUTOMATE_PG_MIGRATE_LOG_DIR, os.ModeDir)
		if err != nil {
			panic(err)
		}
	}
	writer.Printf("%s command execution started \n\n\n", command)

	c := exec.Command(command, args...)
	c.Dir = AUTOMATE_PG_MIGRATE_LOG_DIR
	c.Stdin = os.Stdin

	outfile, err := os.Create(logFilePath)
	if err != nil {
		panic(err)
	}
	defer outfile.Close()
	c.Stdout = outfile
	c.Stderr = outfile
	c.SysProcAttr = &syscall.SysProcAttr{}
	uid, gid, err := lookupUser("hab")
	if err != nil {
		return err
	}
	c.SysProcAttr.Credential = &syscall.Credential{Uid: uint32(uid), Gid: uint32(gid)}

	err = c.Start()
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, helpDocs)
	}
	writer.Printf("%s command execution inprogress with process id : %d, + \n storing log in %s \n", command, c.Process.Pid, logFilePath)
	executed := make(chan struct{})
	go tailFile(logFilePath, executed)
	_, err = c.Process.Wait()
	if err != nil {
		return err
	}
	time.Sleep(5 * time.Second)
	close(executed)
	return err
}

func lookupUser(username string) (uid, gid int, err error) {
	u, err := user.Lookup(username)
	if err != nil {
		return -1, -1, err
	}
	uid, err = strconv.Atoi(u.Uid)
	if err != nil {
		return -1, -1, err
	}
	gid, err = strconv.Atoi(u.Gid)
	if err != nil {
		return -1, -1, err
	}
	return uid, gid, nil
}

func RemoveIndex(s []string, index int) []string {
	return append(s[:index], s[index+1:]...)
}
