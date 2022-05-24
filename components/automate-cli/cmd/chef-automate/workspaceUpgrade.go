package main

import (
	"os/exec"
	"strconv"
	"strings"

	"github.com/blang/semver"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest/parser"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

const habpkgcmd = "HAB_LICENSE=accept-no-persist hab pkg path chef/automate-ha-deployment"
const pkgName = "automate-ha-deployment"

const workspaceUpgradeHelpDocs = `
This command will be used to upgrde Automate HA workspace version.
Usage:
    chef-automate workspace-upgrade <airgap-bundle>
`

func init() {
	workspaceUpgrdeCmd.SetUsageTemplate(workspaceUpgradeHelpDocs)
	RootCmd.AddCommand(workspaceUpgrdeCmd)
}

var workspaceUpgrdeCmd = &cobra.Command{
	Use:   "workspace-upgrade",
	Short: "upgrdeWorkspace",
	Long:  "Upgrade Automate HA workspace version",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
	},
	RunE:   worspaceUpgradeCmdExecute,
	Hidden: true,
}

func worspaceUpgradeCmdExecute(cmd *cobra.Command, args []string) error {
	if len(args) < 1 {
		return status.Wrap(errors.New("Incorrect command usage"), 0, workspaceUpgradeHelpDocs)
	}
	if isA2HARBFileExist() {
		upgraded := upgradeWorspace(args[0])
		if !upgraded {
			return errors.New("No upgrade available in airgap bundle")
		} else {
			writer.Println("upgraded.")
			return nil
		}
	}
	return errors.New(AUTOMATE_HA_INVALID_BASTION)
}

func upgradeWorspace(bundle string) bool {
	updateAvailabe, err := checkIfNewBundleHaveWorkspaceUpdate(bundle)
	if err != nil {
		writer.Println(err.Error())
	}
	if updateAvailabe {
		upgradeAccepted := false
		if !upgradeRunCmdFlags.upgradeHAWorkspace {
			response, err := writer.Prompt("Automate HA workspace will get updated to latest version press y to agree, n to to disagree? [y/n]")
			if err != nil {
				upgradeAccepted = false
			}
			if !strings.Contains(response, "y") {
				upgradeAccepted = false
				writer.Println("Not update HA workspace to newer version.")
			} else {
				upgradeAccepted = true
			}
		} else {
			upgradeAccepted = true
		}
		if upgradeAccepted {
			writer.Println("Bootstraping for new version.")
			err := doBootstrapEnv(bundle)
			if err != nil {
				writer.Println(err.Error())
				return false
			}
			return true
		}
	}
	return false
}
func checkIfNewBundleHaveWorkspaceUpdate(bundle string) (bool, error) {
	currVerAndRel := getCurrentInstalledWorsapceVersion()
	newVersion, err := getWorkspaceVersionFromBundle(bundle)
	if err != nil {
		return false, err
	}
	nv0 := semver.MustParse(newVersion[0])
	cv0 := semver.MustParse(currVerAndRel[0])

	nv1, _ := strconv.ParseInt(strings.TrimSpace(newVersion[1]), 10, 64)
	cv1, _ := strconv.ParseInt(strings.TrimSpace(currVerAndRel[1]), 10, 64)
	if nv0.GT(cv0) || nv1 > cv1 {
		writer.Println("update for new verison of automate-ha-deployment available")
		writer.Printf("Installed version %s, %s \n", currVerAndRel[0], currVerAndRel[1])
		writer.Printf("Airgap bundled version %s, %s \n", newVersion[0], newVersion[1])
		return true, nil
	}
	return false, nil
}

func getCurrentInstalledWorsapceVersion() []string {
	out, err := exec.Command("/bin/sh", "-c", habpkgcmd).Output()
	if err != nil {
		writer.Fail(err.Error())
		return nil
	}
	currentVersion := string(out)
	cvTokens := strings.Split(currentVersion, "/")
	currVerAndRel := cvTokens[len(cvTokens)-2:]
	return currVerAndRel
}

func getWorkspaceVersionFromBundle(bundle string) ([]string, error) {
	_, manifestBytes, err := airgap.GetMetadata(bundle)
	if err != nil {
		return nil, status.Annotate(err, status.AirgapUnpackInstallBundleError)
	}

	manifest, err := parser.ManifestFromBytes(manifestBytes)
	if err != nil {
		return nil, status.Annotate(err, status.AirgapUnpackInstallBundleError)
	}

	writer.Printf("  Version: %s\n", manifest.Build)
	writer.Printf("Build SHA: %s\n", manifest.BuildSHA)
	writer.Printf(" Packages:\n")
	workspacePkgVersion := make([]string, 2)
	for _, pkg := range manifest.Packages {
		_pkgName := pkg.Name()
		if strings.Contains(_pkgName, pkgName) {
			pkg.Release()
			workspacePkgVersion[0] = pkg.Version()
			workspacePkgVersion[1] = pkg.Release()
			//append(workspacePkgVersion, pkg.Version(), pkg.Release())
			break
		}
	}
	return workspacePkgVersion, nil
}
