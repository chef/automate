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
	"github.com/chef/automate/components/automate-deployment/pkg/majorupgradechecklist"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/chef/automate/lib/user"
	"github.com/spf13/cobra"
)

var migrateDataCmdFlags = struct {
	check            bool
	data             string
	autoAccept       bool
	forceExecute     bool
	skipStorageCheck bool
}{}

var ClearDataCmdFlags = struct {
	data         string
	autoAccept   bool
	forceExecute bool
}{}

//changes with postgresql13 version pinned in components/automate-postgresql/habitat/plan.sh
var NEW_BIN_DIR = "/hab/pkgs/core/postgresql13/13.5/20220120092917/bin"

const (
	AUTOMATE_VERSION            = "3"
	AUTOMATE_PG_MIGRATE_LOG_DIR = "/tmp"
	OLD_PG_VERSION              = "9.6"
	OLD_PG_DATA_DIR             = "/hab/svc/automate-postgresql/data/pgdata"
	NEW_PG_DATA_DIR             = "/hab/svc/automate-postgresql/data/pgdata13"
	PG_DATA_DIR                 = "/hab/svc/automate-postgresql/data"
	PGPORT                      = "5432"
	PGHOST                      = "0.0.0.0"
	PGUSER                      = "automate"
	PGDATABASE                  = "postgres"
	PGSSLMODE                   = "verify-ca"
	PGSSLCERT                   = "/hab/svc/automate-postgresql/config/server.crt"
	PGSSLKEY                    = "/hab/svc/automate-postgresql/config/server.key"
	PGSSLROOTCERT               = "/hab/svc/automate-postgresql/config/root.crt"
	OLD_BIN_DIR                 = "/hab/pkgs/core/postgresql/9.6.24/20220218015755/bin"
	CLEANUP_ID                  = "clean_up"
	MIGRATE_PG_ID               = "migrate_pg"
	NEW_PG_VERSION              = "13.5"
)

func init() {
	migrateCmd.AddCommand(newMigrateDataCmd())
	migrateCmd.AddCommand(newClearDataCmd())
	RootCmd.AddCommand(migrateCmd)
}

var migrateCmd = &cobra.Command{
	Use:    "post-major-upgrade COMMAND",
	Short:  "Utilities for post-major-upgrade",
	Hidden: true,
}

func newClearDataCmd() *cobra.Command {
	var clearDataCmd = &cobra.Command{
		Use:   "clear-data",
		Short: "Chef Automate post-major-upgrade clear-data",
		Long:  "Chef Automate post-major-upgrade to clear old pg data",
		RunE:  runCleanup,
	}
	clearDataCmd.PersistentFlags().StringVar(&ClearDataCmdFlags.data, "data", "", "data")
	clearDataCmd.PersistentFlags().BoolVarP(&ClearDataCmdFlags.autoAccept, "", "y", false, "auto-accept")
	clearDataCmd.Flags().BoolVarP(&ClearDataCmdFlags.forceExecute, "force", "f", false, "fore-execute")
	return clearDataCmd
}

func newMigrateDataCmd() *cobra.Command {
	var migrateDataCmd = &cobra.Command{
		Use:   "migrate",
		Short: "Chef Automate post-major-upgrade migrate",
		Long:  "Chef Automate migrate. migrate can be used to migrate pg or migrate es",
		RunE:  runMigrateDataCmd,
	}
	migrateDataCmd.PersistentFlags().BoolVar(&migrateDataCmdFlags.check, "check", false, "check")
	migrateDataCmd.PersistentFlags().StringVar(&migrateDataCmdFlags.data, "data", "", "data")
	migrateDataCmd.PersistentFlags().BoolVarP(&migrateDataCmdFlags.autoAccept, "", "y", false, "auto-accept")
	migrateDataCmd.PersistentFlags().BoolVarP(&migrateDataCmdFlags.skipStorageCheck, "skip-storage-check", "s", false, "skip storage check")
	migrateDataCmd.Flags().BoolVarP(&migrateDataCmdFlags.forceExecute, "force", "f", false, "force-execute")
	return migrateDataCmd
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
			ci, err := majorupgradechecklist.NewPostChecklistManager(AUTOMATE_VERSION)
			if err != nil {
				return err
			}

			isExecuted, err := ci.ReadPostChecklistById(CLEANUP_ID, majorupgradechecklist.UPGRADE_METADATA)
			if err != nil {
				return err
			}

			if isExecuted {
				if ClearDataCmdFlags.forceExecute {
					isExecuted = false
				} else {
					err := promptCheckList(
						"Cleanup is already executed,do you want to force execute.\nPress y to agree, n to disagree? [y/n]",
					)
					if err != nil {
						return err
					} else {
						isExecuted = false
					}
				}
			}

			if !isExecuted {
				writer.Title("Deleting file created by pg_upgrade")
				err := cleanUp()
				if err != nil {
					//nothing
				}
			}
		} else {
			return errors.New("please provide valid input for data flag")
		}

	} else {
		return errors.New(
			"pg migration will only support 9.6 pg version for now, your pg version is: " + oldPgVersion,
		)
	}

	return nil
}

func runMigrateDataCmd(cmd *cobra.Command, args []string) error {
	_, err := calDiskSizeAndDirSize()
	if err != nil {
		return err
	}

	if migrateDataCmdFlags.data == "" {
		return errors.New("data flag is required")
	} else if strings.ToLower(migrateDataCmdFlags.data) == "pg" {
		ci, err := majorupgradechecklist.NewPostChecklistManager(AUTOMATE_VERSION)
		if err != nil {
			return err
		}

		isExecuted, err := ci.ReadPostChecklistById(MIGRATE_PG_ID, majorupgradechecklist.UPGRADE_METADATA)
		if err != nil {
			return err
		}

		if isExecuted {
			if migrateDataCmdFlags.forceExecute {
				isExecuted = false
			} else {
				err := promptCheckList(
					"migrate_pg is already executed,do you want to force execute.\nPress y to agree, n to disagree? [y/n]",
				)
				if err != nil {
					return err
				} else {
					isExecuted = false
				}
			}
		}

		if !isExecuted {
			if !migrateDataCmdFlags.check && !migrateDataCmdFlags.autoAccept {
				err := promptCheckList(
					"It will start the migration immediately after check.\nPress y to agree, n to disagree? [y/n]",
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
					"pg migration will only support 9.6 pg version for now, your pg version is: " + oldPgVersion,
				)
			}
		}
	} else {
		return errors.New("please provide valid input for data flag")
	}
	return nil
}

func pgMigrateExecutor() error {
	upgraded := false
	getLatestPgPath()
	existDir, err := dirExists(NEW_PG_DATA_DIR)
	if err != nil {
		return err
	}

	defer func() {
		err = chefAutomateStart()
		if err != nil {
			fmt.Println(err.Error())
		}
		err = chefAutomateStatus()
		if err != nil {
			fmt.Println(err)
		}
		if !migrateDataCmdFlags.check && err == nil && upgraded {
			err = vacuumDb()
			if err != nil {
				fmt.Println(err)
			}
		}
	}()

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
	upgraded = true
	return nil
}

func getLatestPgPath() {
	latestPgPath := "lsof -F n| grep /hab/pkgs/core/postgresql13 | awk '{print $0}' | awk -F \"/\" '/1/ {print $5\"/\"$6\"/\"$7}' | uniq"
	cmd, err := exec.Command("/bin/sh", "-c", latestPgPath).Output()
	if err != nil {
		fmt.Printf("error %s", err)
	}
	output := string(cmd)

	output = strings.Split(output, "\n")[0]

	if strings.TrimSpace(output) == "" {
		return
	}

	if strings.Contains(strings.ToUpper(output), NEW_PG_VERSION) {
		NEW_BIN_DIR = "/hab/pkgs/core/" + strings.TrimSpace(output) + "/bin"
	} else {
		fmt.Printf("latest version %s not found", NEW_PG_VERSION)
	}
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
		AUTOMATE_PG_MIGRATE_LOG_DIR + "/analyze_new_cluster.sh",
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
		AUTOMATE_PG_MIGRATE_LOG_DIR + "/analyze_new_cluster.sh",
		AUTOMATE_PG_MIGRATE_LOG_DIR + "delete_old_cluster.sh",
		AUTOMATE_PG_MIGRATE_LOG_DIR + "/pgmigrate.log",
		OLD_PG_DATA_DIR,
	}
	err := executeCommand("rm", args, "")
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	} else {
		ci, err := majorupgradechecklist.NewPostChecklistManager(AUTOMATE_VERSION)
		if err != nil {
			return err
		}
		err = ci.UpdatePostChecklistFile(CLEANUP_ID, majorupgradechecklist.UPGRADE_METADATA)
		if err != nil {
		}
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

	input, err := ioutil.ReadFile("/hab/svc/automate-postgresql/hooks/init") // nosemgrep
	if err != nil {
		fmt.Printf("Failed to read init hook file")
		return err
	}

	output := bytes.Replace(input, []byte("initdb"), []byte(NEW_BIN_DIR+"/initdb"), -1)

	if err = ioutil.WriteFile("/tmp/pgdata13.sh", output, 0100755); err != nil { // nosemgrep
		fmt.Printf("Failed to write init hook file")
		return err
	}

	args := []string{
		"/tmp/pgdata13.sh",
	}
	c := exec.Command("/bin/bash", args...)
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
	var isAvailableSpace bool
	var err error

	if migrateDataCmdFlags.skipStorageCheck {
		isAvailableSpace = true
	} else {
		isAvailableSpace, err = calDiskSizeAndDirSize()

		if err != nil {
			return err
		}
	}

	if isAvailableSpace {
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
			AUTOMATE_PG_MIGRATE_LOG_DIR+"/pgmigrate.log")

		if err != nil {
			return err
		}
		if !check && err == nil {
			ci, err := majorupgradechecklist.NewPostChecklistManager(AUTOMATE_VERSION)
			if err != nil {
				return err
			}
			err = ci.UpdatePostChecklistFile(MIGRATE_PG_ID, majorupgradechecklist.UPGRADE_METADATA)
			if err != nil {
			}
		}
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
	fmt.Println(string(out))
	if err != nil {
		return err
	}
	return nil
}

func dirExists(path string) (bool, error) {
	_, err := os.Stat(path)
	if err == nil {
		return true, nil
	}
	if os.IsNotExist(err) { // nosemgrep
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
		return errors.New("cancelled")
	}
	return nil
}

// check pg version from path
func pgVersion(path string) (string, error) {
	data, err := ioutil.ReadFile(path) // nosemgrep
	if err != nil {
		return "", errors.New("could not find pg_version file")
	}

	getOldPgVersion := string(bytes.Trim(data, ""))
	return getOldPgVersion, nil
}

func calDiskSizeAndDirSize() (bool, error) {
	v, err := sys.SpaceAvailForPath(PG_DATA_DIR)
	if err != nil {
		return false, err
	}
	diskSpaceInMb := v / 1024

	size, err := sys.DirSize(OLD_PG_DATA_DIR)
	if err != nil {
		return false, err
	}

	dirSizeInMb := size / (1024 * 1024)

	msg := fmt.Sprintf("Insufficient disk space for migration.\n%s: %5d MB\n%s: %5d MB\n%s",
		"Space Required", dirSizeInMb,
		"Space Available", diskSpaceInMb,
		"To continue with less memory Please use --skip-storage-check")

	if diskSpaceInMb > uint64(dirSizeInMb) {
		return true, nil
	} else {
		return false, errors.New(msg)
	}
}
