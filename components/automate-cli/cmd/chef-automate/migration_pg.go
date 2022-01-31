package main

import (
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/lib/user"
	"github.com/spf13/cobra"
)

var migrateDataCmdFlags = struct {
	check bool
	data  string
}{}

var ClearDataCmdFlags = struct {
	data string
}{}

const AUTOMATE_PG_MIGRATE_LOG_DIR = "/src/"

func init() {
	migrateCmd.AddCommand(newMigratePgCmd())
	migrateCmd.AddCommand(newRemovePgDatadirCmd())
	RootCmd.AddCommand(migrateCmd)
}

var migrateCmd = &cobra.Command{
	Use:    "post-major-upgrade COMMAND",
	Short:  "Utilities for post-major-upgrade",
	Hidden: true,
}

func newRemovePgDatadirCmd() *cobra.Command {
	var removePgDatadirCmd = &cobra.Command{
		Use:   "clear-data",
		Short: "Chef Automate post-major-upgrade clear-data",
		Long:  "Chef Automate migrate_pg. from one version to another",
		RunE:  runCleanup,
	}
	removePgDatadirCmd.PersistentFlags().StringVar(&ClearDataCmdFlags.data, "data", "", "data")

	return removePgDatadirCmd
}

func newMigratePgCmd() *cobra.Command {
	var migratePgCmd = &cobra.Command{
		Use:   "migrate",
		Short: "Chef Automate post-major-upgrade migrate",
		Long:  "Chef Automate migrate. migrate can be used to pg migrate or es migrate",
		RunE:  runMigratePgCmd,
	}
	migratePgCmd.PersistentFlags().BoolVar(&migrateDataCmdFlags.check, "check", false, "check")
	migratePgCmd.PersistentFlags().StringVar(&migrateDataCmdFlags.data, "data", "", "data")
	return migratePgCmd
}

func runCleanup(cmd *cobra.Command, args []string) error {
	writer.Title("Cleanup")
	if ClearDataCmdFlags.data == "" {
		return errors.New("data flag is required")
	} else if strings.ToLower(ClearDataCmdFlags.data) == "pg" {
		writer.Title("Deleting file created by pg_upgrade")
	} else {
		return errors.New("Please provide valid input for data flag")
	}
	cleanUp()
	writer.Title("successfully deleted files")
	return nil
}

func runMigratePgCmd(cmd *cobra.Command, args []string) error {
	if !migrateDataCmdFlags.check {
		response, err := writer.Prompt(`it will start the migration immediately after check.
		Press y to agree, n to disagree? [y/n]`)
		if err != nil {
			return err
		}
		if !strings.Contains(response, "y") {
			return errors.New("canceled")
		}
	}

	if migrateDataCmdFlags.data == "" {
		return errors.New("data flag is required")
	} else if strings.ToLower(migrateDataCmdFlags.data) == "pg" {
		chefAutomateStop()
		removeAndReplacePgdata13()
		executePgdata13ShellScript()
		checkUpdateMigration(migrateDataCmdFlags.check)
		chefAutomateStart()
		chefAutomateStatus()
		if !migrateDataCmdFlags.check {
			vacuumDb()
		}
	} else {
		return errors.New("Plase provide valid input for data flag")
	}
	return nil
}

func vacuumDb() {
	writer.Title("vacuum db")
	writer.Title("--------------------------------")
	os.Setenv("PGPORT", "5432")
	os.Setenv("PGHOST", "0.0.0.0")
	os.Setenv("PGUSER", "automate")
	os.Setenv("PGDATABASE", "postgres")
	os.Setenv("PGSSLMODE", "verify-ca")
	os.Setenv("PGSSLCERT", "/hab/svc/automate-postgresql/config/server.crt")
	os.Setenv("PGSSLKEY", "/hab/svc/automate-postgresql/config/server.key")
	os.Setenv("PGSSLROOTCERT", "/hab/svc/automate-postgresql/config/root.crt")
	args := []string{
		"./analyze_new_cluster.sh",
	}
	executeCommand("/bin/sh", args, "")

}

func cleanUp() error {
	response, err := writer.Prompt(`Are you sure do you want to delete old pg-data
	This will delete all the data in the database (pg 9.6) and will not be able to recover it.
	Press y to agree, n to disagree? [y/n]`)
	if err != nil {
		return err
	}
	if !strings.Contains(response, "y") {
		return errors.New("canceled")
	}
	args := []string{
		"-rf",
		"./analyze_new_cluster.sh",
		"./delete_old_cluster.sh",
		"./pgmigrate.log",
		"/hab/svc/automate-postgresql/data/pgdata",
	}
	executeCommand("rm", args, "")
	return nil
}

func chefAutomateStop() {
	writer.Title("Chef-automate stop")
	writer.Title("--------------------------------")
	args := []string{
		"stop",
	}

	executeCommand("chef-automate", args, "")

}

func chefAutomateStatus() {
	writer.Title("Chef-automate status")
	writer.Title("--------------------------------")
	args := []string{
		"status",
		"--wait-for-healthy",
	}
	executeCommand("chef-automate", args, "")
}

func removeAndReplacePgdata13() {

	writer.Title("remove and replace pgdata13 directory")
	writer.Title("--------------------------------")

	argsToRemove := []string{
		"-rf",
		"/hab/svc/automate-postgresql/data/pgdata13",
	}

	executeCommand("rm", argsToRemove, "")

}

func chefAutomateStart() {
	writer.Title("Chef-automate start")
	writer.Title("--------------------------------")

	args := []string{
		"start",
	}

	executeCommand("chef-automate", args, "")

}

func executePgdata13ShellScript() {
	writer.Title("execute pgdata13 shell script")
	writer.Title("--------------------------------")
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
	writer.Title(" migration from: 9.6 to: 13")
	writer.Title("--------------------------------")

	os.Unsetenv("PGHOST")

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
		"./pgmigrate.log")
	if err != nil {
		fmt.Println(err)
	}
}

func executeCommand(command string, args []string, workingDir string) {
	c := exec.Command(command, args...)
	c.Stdin = os.Stdin
	if len(workingDir) > 0 {
		c.Dir = workingDir
	}
	c.Stdout = io.MultiWriter(os.Stdout)
	c.Stderr = io.MultiWriter(os.Stderr)
	err := c.Run()
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
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

func checkErrorForCommand(executable *exec.Cmd) {
	out, err := executable.Output()
	if err != nil {
		log.Fatal(err)
		os.Exit(1)
	}
	fmt.Println(string(out))
}
