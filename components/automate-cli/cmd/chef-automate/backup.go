package main

import (
	"context"
	"fmt"
	"os"
	"os/user"
	"path"
	"path/filepath"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/backup"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/preflight"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/io/fileutils"
)

var backupCmdFlags = struct {
	noProgress     bool
	requestTimeout int64

	baseBackupDir  string
	channel        string
	overrideOrigin string
	hartifactsPath string
	upgrade        bool
	skipPreflight  bool
	skipBootstrap  bool
	airgap         string
	yes            bool

	createWaitTimeout  int64
	listWaitTimeout    int64
	showWaitTimeout    int64
	deleteWaitTimeout  int64
	restoreWaitTimeout int64
	statusWaitTimeout  int64
	cancelWaitTimeout  int64

	s3Endpoint     string
	s3AccessKey    string
	s3SecretKey    string
	s3SessionToken string

	sha256 string
}{}

func init() {
	backupCmd.AddCommand(createBackupCmd)
	backupCmd.AddCommand(listBackupCmd)
	backupCmd.AddCommand(showBackupCmd)
	backupCmd.AddCommand(deleteBackupCmd)
	backupCmd.AddCommand(restoreBackupCmd)
	backupCmd.AddCommand(fixBackupRepoPermissionsCmd)
	backupCmd.AddCommand(statusBackupCmd)
	backupCmd.AddCommand(cancelBackupCmd)

	backupCmd.PersistentFlags().BoolVarP(&backupCmdFlags.noProgress, "no-progress", "", false, "Don't follow operation progress")
	backupCmd.PersistentFlags().Int64VarP(&backupCmdFlags.requestTimeout, "request-timeout", "r", 20, "API request timeout for deployment-service in seconds")
	backupCmd.PersistentFlags().StringVar(&backupCmdFlags.s3Endpoint, "s3-endpoint", "", "The S3 region endpoint URL")
	backupCmd.PersistentFlags().StringVar(&backupCmdFlags.s3AccessKey, "s3-access-key", "", "The S3 access key ID")
	backupCmd.PersistentFlags().StringVar(&backupCmdFlags.s3SecretKey, "s3-secret-key", "", "The S3 secret access key")
	backupCmd.PersistentFlags().StringVar(&backupCmdFlags.s3SessionToken, "s3-session-token", "", "The S3 session token when assuming an IAM role")

	createBackupCmd.PersistentFlags().Int64VarP(&backupCmdFlags.createWaitTimeout, "wait-timeout", "t", 7200, "How long to wait for a operation to complete before raising an error")

	listBackupCmd.PersistentFlags().Int64VarP(&backupCmdFlags.listWaitTimeout, "wait-timeout", "t", 60, "How long to wait for a operation to complete before raising an error")

	showBackupCmd.PersistentFlags().Int64VarP(&backupCmdFlags.showWaitTimeout, "wait-timeout", "t", 60, "How long to wait for a operation to complete before raising an error")

	statusBackupCmd.PersistentFlags().Int64VarP(&backupCmdFlags.statusWaitTimeout, "wait-timeout", "t", 60, "How long to wait for a operation to complete before raising an error")

	cancelBackupCmd.PersistentFlags().Int64VarP(&backupCmdFlags.cancelWaitTimeout, "wait-timeout", "t", 60, "How long to wait for a operation to complete before raising an error")

	restoreBackupCmd.PersistentFlags().StringVarP(&backupCmdFlags.baseBackupDir, "backup-dir", "b", "/var/opt/chef-automate/backups", "Directory used for backups")
	restoreBackupCmd.PersistentFlags().StringVarP(&backupCmdFlags.overrideOrigin, "override-origin", "o", "chef", "Habitat origin from which to install packages")
	restoreBackupCmd.PersistentFlags().StringVarP(&backupCmdFlags.hartifactsPath, "hartifacts", "", "", "The local path to search for override packages")
	restoreBackupCmd.PersistentFlags().StringVarP(&backupCmdFlags.channel, "channel", "c", "current", "The habitat channel from which to install packages")
	restoreBackupCmd.PersistentFlags().BoolVarP(&backupCmdFlags.upgrade, "upgrade", "u", false, "Upgrade to the latest package versions when restoring backups")
	restoreBackupCmd.PersistentFlags().BoolVarP(&backupCmdFlags.skipPreflight, "skip-preflight", "", false, "Skip preflight checks when restoring a backup")
	restoreBackupCmd.PersistentFlags().BoolVarP(&backupCmdFlags.skipBootstrap, "skip-bootstrap", "", false, "Skip bootstrapping the machine with Habitat")
	restoreBackupCmd.PersistentFlags().StringVar(&backupCmdFlags.airgap, "airgap-bundle", "", "The artifact to use for an air-gapped installation")
	restoreBackupCmd.PersistentFlags().BoolVarP(&backupCmdFlags.yes, "yes", "", false, "Skip bootstrapping the machine with Habitat")
	restoreBackupCmd.PersistentFlags().StringVar(&backupCmdFlags.sha256, "sha256", "", "The SHA256 checksum of the backup")
	restoreBackupCmd.PersistentFlags().Int64VarP(&backupCmdFlags.restoreWaitTimeout, "wait-timeout", "t", 7200, "How long to wait for a operation to complete before raising an error")

	deleteBackupCmd.PersistentFlags().BoolVar(&backupDeleteCmdFlags.yes, "yes", false, "Agree to all prompts")
	deleteBackupCmd.PersistentFlags().Int64VarP(&backupCmdFlags.deleteWaitTimeout, "wait-timeout", "t", 120, "How long to wait for a operation to complete before raising an error")

	if !isDevMode() {
		_ = restoreBackupCmd.PersistentFlags().MarkHidden("override-origin")
		_ = restoreBackupCmd.PersistentFlags().MarkHidden("hartifacts")
		_ = restoreBackupCmd.PersistentFlags().MarkHidden("channel")
		_ = restoreBackupCmd.PersistentFlags().MarkHidden("skip-bootstrap")
	}

	RootCmd.AddCommand(backupCmd)
}

var backupCmd = &cobra.Command{
	Use:   "backup COMMAND",
	Short: "Chef Automate backup",
}

var createBackupCmd = &cobra.Command{
	Use:   "create",
	Short: "create a backup of Chef Automate",
	Long:  "Create a backup of Chef Automate",
	RunE:  runCreateBackupCmd,
	Args:  cobra.MaximumNArgs(0),
}

type createBackupResult struct {
	BackupId string `json:"backup_id"`
	SHA256   string `json:"sha256"`
}

var listBackupCmd = &cobra.Command{
	Use:   "list",
	Short: "list all Chef Automate backups",
	Long:  "List all Chef Automate backups",
	RunE:  runListBackupCmd,
	Args:  cobra.MaximumNArgs(1),
}

var showBackupCmd = &cobra.Command{
	Use:   "show ID",
	Short: "show the Chef Automate backup details",
	Long:  "Show the details of a Chef Automate backup",
	RunE:  runShowBackupCmd,
	Args:  cobra.ExactArgs(1),
}

var backupDeleteCmdFlags = struct {
	yes bool
}{}

var deleteBackupCmd = &cobra.Command{
	Use:   "delete ID [ID2 IDN...]",
	Short: "delete backups of Chef Automate",
	Long:  "Delete one or many backups of Chef Automate that match the space separated strings of backup IDs",
	RunE:  runDeleteBackupCmd,
}

var restoreBackupCmd = &cobra.Command{
	Use:   "restore ID_OR_PATH",
	Short: "restore a Chef Automate backup",
	Long:  "Restore a Chef Automate backup",
	RunE:  runRestoreBackupCmd,
	Args:  cobra.ExactArgs(1),
}

var fixBackupRepoPermissionsCmd = &cobra.Command{
	Use:   "fix-repo-permissions PATH",
	Short: "Ensure the hab user has the required permissions on the given path",
	Long:  "Ensure the hab user has the required permissions on the given path",
	RunE:  runFixBackupRepoPermissionsCmd,
	Args:  cobra.ExactArgs(1),
}

var statusBackupCmd = &cobra.Command{
	Use:   "status",
	Short: "show the Chef Automate backup runner status",
	Long:  "Show the Chef Automate backup runner status",
	RunE:  runBackupStatusCmd,
	Args:  cobra.ExactArgs(0),
}

var cancelBackupCmd = &cobra.Command{
	Use:   "cancel",
	Short: "cancel the running backup operation",
	Long:  "Cancel the currently running backup create, delete, or restore operation",
	RunE:  runCancelBackupCmd,
	Args:  cobra.ExactArgs(0),
}

func runCreateBackupCmd(cmd *cobra.Command, args []string) error {
	res, err := client.CreateBackup(
		time.Duration(backupCmdFlags.requestTimeout)*time.Second,
		time.Duration(backupCmdFlags.createWaitTimeout)*time.Second,
		writer,
	)
	if err != nil {
		return err
	}

	status.GlobalResult = createBackupResult{
		BackupId: res.Backup.TaskID(),
	}

	if backupCmdFlags.noProgress {
		writer.Println(res.Backup.TaskID())
		return nil
	}
	lastEvent, err := client.StreamBackupStatus(
		time.Duration(backupCmdFlags.requestTimeout)*time.Second,
		time.Duration(backupCmdFlags.createWaitTimeout)*time.Second,
		res.Backup.TaskID(),
		writer,
	)
	if err != nil {
		return status.Wrap(err, status.BackupError, "Streaming backup events failed")
	}

	if lastEvent != nil && lastEvent.GetBackup() != nil {
		status.GlobalResult = createBackupResult{
			BackupId: res.Backup.TaskID(),
			SHA256:   lastEvent.GetBackup().Description.Sha256,
		}
	}

	writer.Successf("Created backup %s", res.Backup.TaskID())
	return nil
}

func parseLocationSpecFromCLIArgs(location string) (backup.LocationSpecification, error) {
	if strings.HasPrefix(location, "s3://") {
		bucketAndBasePath := strings.TrimPrefix(location, "s3://")
		parts := strings.SplitN(bucketAndBasePath, "/", 2)
		bucketName := ""
		basePath := ""
		if len(parts) == 1 {
			bucketName = parts[0]
		} else if len(parts) == 2 {
			bucketName = parts[0]
			basePath = parts[1]
		} else {
			return nil, status.Errorf(status.InvalidCommandArgsError,
				"%q could not be parsed. The expected input is s3://bucket/base/path",
				location,
			)
		}

		return backup.S3LocationSpecification{
			BucketName:   bucketName,
			BasePath:     basePath,
			Endpoint:     backupCmdFlags.s3Endpoint,
			AccessKey:    backupCmdFlags.s3AccessKey,
			SecretKey:    backupCmdFlags.s3SecretKey,
			SessionToken: backupCmdFlags.s3SessionToken,
		}, nil
	}

	// Make sure that the backup location exists and is read/writable by hab
	fqBackupDir, err := filepath.Abs(location)
	if err != nil {
		return nil, status.Annotate(err, status.FileAccessError)
	}

	// User 'hab' needs read to list the directory, write to create backups,
	// and exec down the entire directory tree to actually access files.
	ok, err := fileutils.ReadWriteExecutable("hab", fqBackupDir)
	if err != nil {
		_, ok := err.(user.UnknownUserError)
		if ok {
			return nil, status.Wrapf(err, status.HabUserAccessError, createHabUserHelpMsg, fqBackupDir)
		}

		return nil, status.Wrapf(err, status.HabUserAccessError, chownChmodBackupDirHelpMsg, fqBackupDir)
	}
	if !ok {
		return nil, status.Errorf(status.HabUserAccessError, chownChmodBackupDirHelpMsg, fqBackupDir)
	}

	return backup.FilesystemLocationSpecification{
		Path: fqBackupDir,
	}, nil
}

var createHabUserHelpMsg = `The 'hab' user was not found. Please ensure the 'hab' user and group exist and
that the 'hab' user has read/write/exec permissions for the backup repository path: %[1]s

If you prefer not altering the ownership and permissions manually, please run the
following command and then try re-running the backup command:

  chef-automate backup fix-repo-permissions %[1]s
`

var chownChmodBackupDirHelpMsg = `The 'hab' user does not have read/write/exec permissions on the backup repository
path: %[1]s. Please update the permissions.

If you prefer not altering the ownership and permissions manually, please run the
following command and then try re-running the backup command:

  chef-automate backup fix-repo-permissions %[1]s
`

func runFixBackupRepoPermissionsCmd(cmd *cobra.Command, args []string) error {
	var (
		path string
		err  error
		t    *target.LocalTarget
	)

	path, err = filepath.Abs(args[0])
	if err != nil {
		return status.Annotate(err, status.FileAccessError)
	}

	t = target.NewLocalTarget(true)
	err = t.EnsureHabUser(writer)
	if err != nil {
		return status.Annotate(err, status.HabUserAccessError)
	}

	err = fileutils.MakeReadWriteExecutable("hab", path)
	if err != nil {
		return status.Annotate(err, status.HabUserAccessError)
	}

	return nil
}

func listBackupsLocally(ctx context.Context, locationSpec backup.LocationSpecification) ([]*api.BackupTask, error) {
	deadline, ok := ctx.Deadline()
	if !ok {
		deadline = time.Now().Add(1 * time.Minute)
	}
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()

	runner := backup.NewRunner(backup.WithBackupLocationSpecification(locationSpec))
	return runner.ListBackups(ctx)
}

var offlineHelpMsg = `The deployment-service failed to respond to our request to list
backups. If you are attempting to list the backups for an off-line
Chef Automate installation, please provide the path to your Chef
Automate backup data directory:

    %s backup list /path/to/backups

`

type listBackupsResult struct {
	Backups []api.FormattedBackupTask `json:"backups"`
}

func runListBackupCmd(cmd *cobra.Command, args []string) error {
	var backups []*api.BackupTask

	if len(args) > 0 {
		locationSpec, err := parseLocationSpecFromCLIArgs(args[0])
		if err != nil {
			return status.Annotate(err, status.BackupError)
		}

		ctx, cancel := context.WithTimeout(context.Background(), time.Duration(backupCmdFlags.listWaitTimeout)*time.Second)
		defer cancel()

		writer.Printf("Listing backups from %s\n", locationSpec)
		backups, err = listBackupsLocally(ctx, locationSpec)
		if err != nil {
			return status.Wrapf(
				err,
				status.BackupError,
				"Listing local backup directory %s failed",
				args[0],
			)
		}
	} else {
		res, err := client.ListBackups(
			time.Duration(backupCmdFlags.requestTimeout)*time.Second,
			time.Duration(backupCmdFlags.listWaitTimeout)*time.Second,
		)
		if err != nil {
			if errors.Cause(err) == context.DeadlineExceeded {
				err = status.Wrapf(
					err,
					status.BackupError,
					offlineHelpMsg,
					os.Args[0],
				)
			}
			return status.Annotate(err, status.BackupError)
		}
		backups = res.Backups
	}

	formattedBackups := make([]api.FormattedBackupTask, len(backups))
	for i, backup := range backups {
		formattedBackups[i] = backup.Formatted()
	}
	status.GlobalResult = listBackupsResult{
		Backups: formattedBackups,
	}

	// TODO(ssd) 2018-04-18: How do we want this sorted?
	writer.Printf("%14s  %11s  %s\n", "Backup", "State", "Age")
	for _, b := range backups {
		var state string
		switch b.State {
		case api.BackupTask_COMPLETED:
			state = "completed"
		case api.BackupTask_IN_PROGRESS:
			state = "in progress"
		case api.BackupTask_FAILED:
			state = "failed"
		case api.BackupTask_DELETING:
			state = "deleting"
		default:
			state = "unknown"
		}

		timestamp, err := ptypes.Timestamp(b.Id)
		if err != nil {
			writer.Printf("%s  %s  bad timestamp: %v", b.TaskID(), state, err.Error())
		}

		age := time.Since(timestamp)
		var timeDesc string
		if age.Hours() > 24 {
			days := int64(age.Hours() / 24)
			timeDesc = fmt.Sprintf("%d day%s old", days, maybeS(days))
		} else if age.Hours() > 1 {
			hours := int64(age.Hours())
			timeDesc = fmt.Sprintf("%d hour%s old", hours, maybeS(hours))
		} else if age.Minutes() > 1 {
			minutes := int64(age.Minutes())
			timeDesc = fmt.Sprintf("%d minute%s old", minutes, maybeS(minutes))
		} else if age.Seconds() > 1 {
			seconds := int64(age.Seconds())
			timeDesc = fmt.Sprintf("%d second%s old", seconds, maybeS(seconds))
		} else {
			timeDesc = "Just created!"
		}

		writer.Printf("%14s  %11s  %s\n", b.TaskID(), state, timeDesc)
	}

	return nil
}

func maybeS(value int64) string {
	if value == 1 || value == -1 {
		return ""
	}
	return "s"
}

func runShowBackupCmd(cmd *cobra.Command, args []string) error {
	id, err := api.NewBackupTaskFromID(args[0])
	if err != nil {
		return status.Wrapf(
			err,
			status.InvalidCommandArgsError,
			"Converting %s into a backup ID failed",
			args[0],
		)
	}

	res, err := client.ShowBackup(
		time.Duration(backupCmdFlags.requestTimeout)*time.Second,
		time.Duration(backupCmdFlags.showWaitTimeout)*time.Second,
		id,
	)
	if err != nil {
		return err
	}

	writer.Titlef("Backup %s:", res.Description.Id)
	// backup integration test uses `cut` to get the SHA256 from this output. You
	// will need to fix that if you modify this.
	writer.Bodyf("SHA256: %s", res.Description.Sha256)
	return nil
}

func runBackupStatusCmd(cmd *cobra.Command, args []string) error {
	res, err := client.BackupStatus(
		time.Duration(backupCmdFlags.requestTimeout)*time.Second,
		time.Duration(backupCmdFlags.statusWaitTimeout)*time.Second,
	)
	if err != nil {
		return err
	}

	writer.Println(res.Format())
	return nil
}

func runCancelBackupCmd(cmd *cobra.Command, args []string) error {
	_, err := client.CancelBackup(
		time.Duration(backupCmdFlags.requestTimeout)*time.Second,
		time.Duration(backupCmdFlags.cancelWaitTimeout)*time.Second,
	)
	if err != nil {
		return err
	}

	writer.Println("Backup operation cancelled")
	return nil
}

func runDeleteBackupCmd(cmd *cobra.Command, args []string) error {
	if len(args) == 0 {
		return status.New(
			status.InvalidCommandArgsError,
			"Must specify a backup",
		)
	}

	ids := make([]*api.BackupTask, len(args))
	for i, arg := range args {
		id, err := api.NewBackupTaskFromID(arg)
		if err != nil {
			return status.Wrapf(
				err,
				status.InvalidCommandArgsError,
				"Converting %s into a backup ID failed",
				arg,
			)
		}
		ids[i] = id
	}

	if !backupDeleteCmdFlags.yes {
		yes, err := writer.Confirm(
			fmt.Sprintf("The following backups will be permanently deleted:\n%s\nAre you sure you want to continue?",
				strings.Join(args, "\n"),
			),
		)
		if err != nil {
			return status.Annotate(err, status.BackupError)
		}
		if !yes {
			return status.New(status.InvalidCommandArgsError, "failed to confirm backup deletion")
		}
	}

	_, err := client.DeleteBackups(
		time.Duration(backupCmdFlags.requestTimeout)*time.Second,
		time.Duration(backupCmdFlags.deleteWaitTimeout)*time.Second,
		ids,
	)
	if err != nil {
		return err
	}
	writer.Success("Backups deleted")
	return nil
}

type restoreBackupResponse struct {
	RestoreId string `json:"restore_id"`
}

var isA2DeployedErrMsg = `
Could not determine if A2 is already deployed. If you would like to continue,
rerun the command with --skip-preflight
`

func isA2Deployed() (bool, error) {
	reporter := preflight.NewInMemoryPrintReporter()
	probe := preflight.NewTestProbe(reporter)
	isA2DeployedCheck := preflight.IsA2DeployedCheck(nil)
	err := isA2DeployedCheck.TestFunc(probe)
	if err != nil {
		logrus.WithError(err).Debug("failed to detect running A2")
		return false, status.Wrap(err, status.BackupRestoreError, isA2DeployedErrMsg)
	}

	return reporter.HasFailures(), nil
}

var a2AlreadyDeployedMessage = `
An existing installation of A2 was detected. The restore process
will modify the existing data to match the contents of the backup.
Are you sure you want to continue?
`

func runRestoreBackupCmd(cmd *cobra.Command, args []string) error {
	if !backupCmdFlags.yes && !backupCmdFlags.skipPreflight {
		deployed, err := isA2Deployed()
		if err != nil {
			return nil
		}

		if deployed {
			yes, err := writer.Confirm(strings.TrimSpace(a2AlreadyDeployedMessage))
			if err != nil {
				return status.Annotate(err, status.BackupError)
			}
			if !yes {
				return status.New(status.InvalidCommandArgsError, "A2 is currently deployed")
			}
		}
	}

	deadline := time.Now().Add(time.Duration(backupCmdFlags.restoreWaitTimeout) * time.Second)
	ctx, cancel := context.WithDeadline(context.Background(), deadline)
	defer cancel()

	// uri will look something like:
	// s3://bucketname/foo/bar/20180901000000
	// or
	// 20180901000000
	// or
	// /path/to/20180901000000
	uri := args[0]

	// Get the backup id: from the example above,
	// all will be 20180901000000
	backupId := path.Base(uri)

	// Remove the backup id from the uri
	// s3://bucketname/foo/bar/20180901000000 => s3://bucketname/foo/bar/
	// or
	// 20180901000000 => ""
	// or
	// /path/to/20180901000000 => /path/to
	location := strings.TrimSuffix(uri, backupId)

	// In the case only the backup id was provided, default the path to
	// baseBackupDir (/var/opt/chef-automate/backups)
	if location == "" {
		location = backupCmdFlags.baseBackupDir
	}

	locationSpec, err := parseLocationSpecFromCLIArgs(location)
	if err != nil {
		return err
	}

	rt, err := api.NewBackupRestoreTaskFromBackupID(backupId)
	if err != nil {
		return status.Wrapf(
			err,
			status.InvalidCommandArgsError,
			"Converting %s into a backup ID failed",
			backupId,
		)
	}

	rt.Upgrade = backupCmdFlags.upgrade
	rt.OverrideOrigin = backupCmdFlags.overrideOrigin
	rt.Channel = backupCmdFlags.channel
	rt.Airgap = backupCmdFlags.airgap != ""
	rt.Sha256 = backupCmdFlags.sha256

	if err := locationSpec.ConfigureBackupRestoreTask(rt); err != nil {
		return status.Annotate(err, status.MarshalError)
	}

	if backupCmdFlags.hartifactsPath != "" {
		fqHartifactsPath, err := filepath.Abs(backupCmdFlags.hartifactsPath)
		if err != nil {
			return status.Annotate(err, status.FileAccessError)
		}
		rt.HartifactsPath = fqHartifactsPath
	}

	// Find matching backup
	backups, err := listBackupsLocally(ctx, locationSpec)
	if err != nil {
		return status.Wrap(err, status.BackupRestoreError, "Listing backups failed")
	}

	found := false
	for _, i := range backups {
		if i.TaskID() == rt.Backup.TaskID() {
			found = true
		}
	}

	if !found {
		return status.Errorf(status.BackupRestoreError,
			"backup with id %s was not found in %s",
			rt.Backup.TaskID(),
			locationSpec,
		)
	}

	// Bootstrap and restore the deployment-service so that we can continue
	// the restoration server side.

	// TODO: If we don't want progress the only thing we'll want to flush
	// to STDOUT is the restore task ID after we've started the server side
	// restore. In that case we need to silence all non-error output in the
	// deployment restore.
	dsRestore := client.NewDeploymentRestore(
		client.WithDeploymentRestoreTask(rt),
		client.WithDeploymentRestoreSkipPreflight(backupCmdFlags.skipPreflight),
		client.WithDeploymentRestoreSkipBootstrap(backupCmdFlags.skipBootstrap),
		client.WithDeploymentRestoreWriter(writer),
		client.WithDeploymentRestoreAirgapInstallBundle(backupCmdFlags.airgap),
	)

	if err := dsRestore.Restore(ctx); err != nil {
		return status.Annotate(err, status.BackupError)
	}

	// Return the task ID if we don't want to follow restore updates
	if backupCmdFlags.noProgress {
		res, err := client.RestoreBackup(
			time.Duration(backupCmdFlags.requestTimeout)*time.Second,
			time.Duration(backupCmdFlags.restoreWaitTimeout)*time.Second,
			rt,
		)
		if err != nil {
			return err
		}

		status.GlobalResult = restoreBackupResponse{
			RestoreId: res.Restore.TaskID(),
		}

		writer.Println(res.Restore.TaskID())
		return nil
	}

	writer.Title("Restoring Chef Automate")
	res, err := client.RestoreBackup(
		time.Duration(backupCmdFlags.requestTimeout)*time.Second,
		time.Until(deadline),
		rt,
	)
	if err != nil {
		return err
	}

	status.GlobalResult = restoreBackupResponse{
		RestoreId: res.Restore.TaskID(),
	}

	writer.Body("Connecting to deployment-service to finish restoration")
	_, err = client.StreamBackupStatus(
		time.Duration(backupCmdFlags.requestTimeout)*time.Second,
		time.Until(deadline),
		res.Restore.TaskID(),
		writer,
	)
	if err != nil {
		return err
	}

	writer.Successf("Restored backup %s", res.Restore.Backup.TaskID())
	return nil
}
