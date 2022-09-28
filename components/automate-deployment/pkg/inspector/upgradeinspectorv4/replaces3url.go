package upgradeinspectorv4

import (
	"fmt"
	"regexp"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/fatih/color"
)

type ReplaceS3UrlInspection struct {
	writer          *cli.Writer
	upgradeUtils    UpgradeV4Utils
	timeout         int64
	exitError       error
	exitedWithError bool
}

const (
	S3_REGEX   = "https?://s3.(.*).amazonaws.com"
	NEW_S3_URL = "https://s3.amazonaws.com"
)

func NewReplaceS3UrlInspection(w *cli.Writer, utls UpgradeV4Utils, timeout int64) *ReplaceS3UrlInspection {
	return &ReplaceS3UrlInspection{
		writer:       w,
		upgradeUtils: utls,
		timeout:      timeout,
	}
}

func (ru *ReplaceS3UrlInspection) ShowInfo(index *int) error {
	return nil
}
func (ru *ReplaceS3UrlInspection) Skip() {
	return
}
func (ru *ReplaceS3UrlInspection) GetShortInfo() []string {
	return nil
}

func (ru *ReplaceS3UrlInspection) Inspect() (err error) {
	endpoint, err := ru.upgradeUtils.GetBackupS3URL(ru.timeout)
	if err != nil {
		ru.setExitError(err)
		return err
	}
	re := regexp.MustCompile(S3_REGEX)
	if re.MatchString(endpoint) {
		_, _, err := ru.upgradeUtils.PatchS3backupURL(ru.timeout)
		if err != nil {
			ru.setExitError(err)
			return err
		}
	}
	return nil
}

func (ru *ReplaceS3UrlInspection) setExitError(err error) {
	ru.exitError = err
	ru.exitedWithError = true
}

func (ru *ReplaceS3UrlInspection) GetInstallationType() inspector.InstallationType {
	return inspector.BOTH
}

func (ru *ReplaceS3UrlInspection) ExitHandler() error {
	if ru.exitedWithError {
		ru.writer.Println(fmt.Errorf("["+color.New(color.FgRed).Sprint("Error")+"] %w", ru.exitError).Error())
	}
	return nil
}
