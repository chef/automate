package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
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
	check      bool
	data       string
	autoAccept bool
}{}

var ClearDataCmdFlags = struct {
	data       string
	autoAccept bool
}{}

var NEW_BIN_DIR = "/hab/pkgs/core/postgresql13/13.5/20220120092917/bin"

const (
	AUTOMATE_PG_MIGRATE_LOG_DIR = "/tmp"
	OLD_PG_VERSION              = "9.6"
	NEW_PG_VERSION              = "13.5"
	OLD_PG_DATA_DIR             = "/hab/svc/automate-postgresql/data/pgdata"
	NEW_PG_DATA_DIR             = "/hab/svc/automate-postgresql/data/pgdata13"
	PGPORT                      = "5432"
	PGHOST                      = "0.0.0.0"
	PGUSER                      = "automate"
	PGDATABASE                  = "postgres"
	PGSSLMODE                   = "verify-ca"
	PGSSLCERT                   = "/hab/svc/automate-postgresql/config/server.crt"
	PGSSLKEY                    = "/hab/svc/automate-postgresql/config/server.key"
	PGSSLROOTCERT               = "/hab/svc/automate-postgresql/config/root.crt"
	OLD_BIN_DIR                 = "/hab/pkgs/core/postgresql/9.6.21/20211016180117/bin"
)

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
		Long:  "Chef Automate post-major-upgrade to clear old pg data",
		RunE:  runCleanup,
	}
	removePgDatadirCmd.PersistentFlags().StringVar(&ClearDataCmdFlags.data, "data", "", "data")
	removePgDatadirCmd.PersistentFlags().BoolVarP(&ClearDataCmdFlags.autoAccept, "", "y", false, "auto-accept")

	return removePgDatadirCmd
}

func newMigratePgCmd() *cobra.Command {
	var migratePgCmd = &cobra.Command{
		Use:   "migrate",
		Short: "Chef Automate post-major-upgrade migrate",
		Long:  "Chef Automate migrate. migrate can be used to migrate pg or migrate es",
		RunE:  runMigratePgCmd,
	}
	migratePgCmd.PersistentFlags().BoolVar(&migrateDataCmdFlags.check, "check", false, "check")
	migratePgCmd.PersistentFlags().StringVar(&migrateDataCmdFlags.data, "data", "", "data")
	migratePgCmd.PersistentFlags().BoolVarP(&migrateDataCmdFlags.autoAccept, "", "y", false, "auto-accept")
	return migratePgCmd
}

func runCleanup(cmd *cobra.Command, args []string) error {
	oldPgVersion, err := pgVersion(OLD_PG_DATA_DIR + "/PG_VERSION")
	if err != nil {
		return err
	}
	if strings.TrimSpace(oldPgVersion) == OLD_PG_VERSION {
		writer.Title(
			"----------------------------------------------\n" +
				"Cleanup \n" +
				"----------------------------------------------",
		)
		if ClearDataCmdFlags.data == "" {
			return errors.New("data flag is required")
		} else if strings.ToLower(ClearDataCmdFlags.data) == "pg" {
			writer.Title("Deleting file created by pg_upgrade")
		} else {
			return errors.New("please provide valid input for data flag")
		}
		cleanUp()

	} else {
		return errors.New(
			"pg migration will only support 9.6 pg version for now, your pg version is: " + string(oldPgVersion),
		)
	}

	return nil
}

func runMigratePgCmd(cmd *cobra.Command, args []string) error {

	if migrateDataCmdFlags.data == "" {
		return errors.New("data flag is required")
	} else if strings.ToLower(migrateDataCmdFlags.data) == "pg" {

		if !migrateDataCmdFlags.check && !migrateDataCmdFlags.autoAccept {
			err := promptCheckList(
				"it will start the migration immediately after check.\nPress y to agree, n to disagree? [y/n]",
			)
			if err != nil {
				return err
			}
		}

		oldPgVersion, err := pgVersion(OLD_PG_DATA_DIR + "/PG_VERSION")
		if err != nil {
			return err
		}

		if strings.TrimSpace(oldPgVersion) == OLD_PG_VERSION {

			err = pgMigrateExecutor()
			if err != nil {
				return err
			}

		} else {
			return errors.New(
				"pg migration will only support 9.6 pg version for now, your pg version is: " + string(oldPgVersion),
			)
		}

	} else {
		return errors.New("please provide valid input for data flag")
	}
	return nil
}

func pgMigrateExecutor() error {
	err := getLatestPgPath()
	if err != nil {
		return err
	}

	existDir, err := dirExists(NEW_PG_DATA_DIR)
	if err != nil {
		return err
	}

	err = chefAutomateStop()
	if err != nil {
		return err
	}

	if existDir {
		err = removeAndReplacePgdata13()
		if err != nil {
			return err
		}
	}

	err = executePgdata13ShellScript()
	if err != nil {
		return err
	}

	err = checkUpdateMigration(migrateDataCmdFlags.check)
	if err != nil {
		return err
	}
	err = chefAutomateStart()
	if err != nil {
		return err
	}
	err = chefAutomateStatus()
	if err != nil {
		return err
	}

	if !migrateDataCmdFlags.check && err == nil {
		err = vacuumDb()
		if err != nil {
			return err
		}
	}

	return nil
}
func getLatestPgPath() error {
	latestPgPath := "find /hab/pkgs/core -name postgresql -exec lsof {} \\; | grep postgresql | awk '{print $2}' | uniq"
	cmd, err := exec.Command("/bin/sh", "-c", latestPgPath).Output()
	if err != nil {
		fmt.Printf("error %s", err)
	}
	output := string(cmd)
	if strings.Contains(strings.ToUpper(output), NEW_PG_VERSION) {
		output = output[:len(output)-10]
		NEW_BIN_DIR = output
	} else {
		return fmt.Errorf("latest version %s not found", NEW_PG_VERSION)
	}
	return nil
}

func vacuumDb() error {
	writer.Title(
		"----------------------------------------------\n" +
			"vacuum db \n" +
			"----------------------------------------------",
	)
	os.Setenv("PGPORT", PGPORT)
	os.Setenv("PGHOST", PGHOST)
	os.Setenv("PGUSER", PGUSER)
	os.Setenv("PGDATABASE", PGDATABASE)
	os.Setenv("PGSSLMODE", PGSSLMODE)
	os.Setenv("PGSSLCERT", PGSSLCERT)
	os.Setenv("PGSSLKEY", PGSSLKEY)
	os.Setenv("PGSSLROOTCERT", PGSSLROOTCERT)

	args := []string{
		"/temp/analyze_new_cluster.sh",
	}

	err := executeCommand("/bin/sh", args, "")
	if err != nil {
		return err
	}
	return nil
}

func cleanUp() error {

	if !migrateDataCmdFlags.autoAccept {
		err := promptCheckList(
			"Are you sure do you want to delete old pg-data\n" +
				"This will delete all the data (pg 9.6) and will not be able to recover it.\n" +
				"Press y to agree, n to disagree? [y/n]")
		if err != nil {
			return err
		}
	}

	args := []string{
		"-rf",
		"/temp/analyze_new_cluster.sh",
		"/temp/delete_old_cluster.sh",
		"/temp/pgmigrate.log",
	}
	err := executeCommand("rm", args, "")
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	} else {
		writer.Title("successfully deleted files")
	}
	return nil
}

func chefAutomateStop() error {
	writer.Title(
		"----------------------------------------------\n" +
			"Chef-automate stop \n" +
			"----------------------------------------------",
	)
	args := []string{
		"stop",
	}

	err := executeCommand("chef-automate", args, "")

	if err != nil {

		if err.Error() == "exit status 99" { // exit status 99 means already stopped
			writer.Warn("chef-automate already stopped")
		} else {
			writer.Fail("chef-automate stop failed")
			return err
		}

	}
	return nil

}

func chefAutomateStatus() error {
	writer.Title(
		"----------------------------------------------\n" +
			"Chef-automate status \n" +
			"----------------------------------------------",
	)
	args := []string{
		"status",
		"--wait-for-healthy",
	}
	err := executeCommand("chef-automate", args, "")
	if err != nil {
		return err
	}
	return nil
}

func removeAndReplacePgdata13() error {
	writer.Title(
		"----------------------------------------------\n" +
			"pgdata13 initDb \n" +
			"----------------------------------------------",
	)
	argsToRemove := []string{
		"-rf",
		NEW_PG_DATA_DIR,
	}

	err := executeCommand("rm", argsToRemove, "")
	if err != nil {
		return err
	}
	return nil
}

func chefAutomateStart() error {
	writer.Title(
		"----------------------------------------------\n" +
			"Chef-automate start \n" +
			"----------------------------------------------",
	)

	args := []string{
		"start",
	}

	err := executeCommand("chef-automate", args, "")
	if err != nil {
		return err
	}
	return nil
}

func executePgdata13ShellScript() error {
	writer.Title(
		"----------------------------------------------\n" +
			"execute pgdata13 shell script \n" +
			"----------------------------------------------",
	)
	args := []string{
		"./scripts/pgdata13.sh",
	}
	c := exec.Command("/bin/sh", args...)
	c.SysProcAttr = &syscall.SysProcAttr{}
	uid, gid, err := lookupUser("hab")
	if err != nil {
		fmt.Printf("failed fetching hab uid and gid: %s\n", err.Error())
		return err
	}
	c.SysProcAttr.Credential = &syscall.Credential{Uid: uint32(uid), Gid: uint32(gid)}
	err = checkErrorForCommand(c)
	if err != nil {
		return err
	}
	return nil
}

func checkUpdateMigration(check bool) error {
	writer.Title(
		"----------------------------------------------\n" +
			"migration from: 9.6 to: 13 \n" +
			"----------------------------------------------",
	)

	os.Unsetenv("PGHOST")

	writer.Title("Checking for pg_upgrade")

	args := []string{
		"--old-datadir=" + OLD_PG_DATA_DIR,
		"--new-datadir=" + NEW_PG_DATA_DIR,
		"--old-bindir=" + OLD_BIN_DIR,
		"--new-bindir=" + NEW_BIN_DIR,
		"--check",
		"-U",
		PGUSER,
	}

	if !check {
		strSlice := removeIndex(args, 4)
		args = strSlice
	}
	err := executeAutomateCommandAsync(
		NEW_BIN_DIR+"/pg_upgrade",
		args,
		"",
		AUTOMATE_PG_MIGRATE_LOG_DIR+"/"+"pgmigrate.log")

	if err != nil {
		return err
	}
	return nil
}

func executeCommand(command string, args []string, workingDir string) error {
	c := exec.Command(command, args...)
	c.Stdin = os.Stdin
	if len(workingDir) > 0 {
		c.Dir = workingDir
	}
	c.Stdout = io.MultiWriter(os.Stdout)
	c.Stderr = io.MultiWriter(os.Stderr)
	err := c.Run()
	return err
}

func executeAutomateCommandAsync(command string, args []string, helpDocs string, logFilePath string) error {
	if len(command) < 1 {
		return errors.New("invalid or empty command")
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

func removeIndex(s []string, index int) []string {
	return append(s[:index], s[index+1:]...)
}

func checkErrorForCommand(executable *exec.Cmd) error {
	out, err := executable.Output()
	if err != nil {
		return err
	}
	fmt.Println(string(out))
	return nil
}

func dirExists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err == nil {
		return true, nil
	}
	if os.IsNotExist(err) {
		return false, nil
	}
	return false, err
}

// prompt checklist
func promptCheckList(message string) error {
	response, err := writer.Prompt(message)
	if err != nil {
		return err
	}
	if !strings.Contains(strings.ToUpper(response), "Y") {
		return errors.New("canceled")
	}
	return nil
}

// check pg version
func pgVersion(path string) (string, error) {
	data, err := ioutil.ReadFile(path)
	if err != nil {
		return "", errors.New("could not find pg_version file")
	}

	getOldPgVersion := string(bytes.Trim(data, ""))
	return getOldPgVersion, nil
}
