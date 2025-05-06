package upgradeinspectorv5

import (
	"errors"
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/fatih/color"
)

const (
	MIN_HAB_FREE_SPACE float64 = 5.5
	MIN_REQ_SPACE_STR          = "The %s directory should have at least %.1fGB of free space"
	ENSURE_SPACE               = "\nPlease ensure the available free space is %.1fGB\nand run "
	UPGRADE_CMD                = "chef-automate upgrade run --major"
	OS_DATA                    = "/svc/automate-opensearch/data"
	PG_DATA                    = "/svc/automate-postgresql/data/pgdata13"
)

type DiskSpaceInspection struct {
	writer              *cli.Writer
	isExternalOS        bool
	isExternalPG        bool
	osDestDir           string
	pgDestDir           string
	fileutils           fileutils.FileUtils
	requiredHabSpace    float64
	requiredOSDestSpace float64
	requiredPGDestSpace float64
	currentHabSpace     float64
	currentSpaceInOSDir float64
	currentSpaceInPGDir float64
	habDir              string
	spinnerHab          *spinner.Spinner
	spinnerOSDestDir    *spinner.Spinner
	spinnerPGDestDir    *spinner.Spinner
	checkDelay          time.Duration
	exitError           error
	exitedWithError     bool
}

func (ds *DiskSpaceInspection) ShowInfo(index *int) (err error) {
	ds.habDir = ds.fileutils.GetHabRootPath()
	ds.currentHabSpace, err = ds.fileutils.GetFreeSpaceinGB(ds.habDir)
	if err != nil {
		return err
	}
	var osDataSize, pgDataSize float64
	ds.requiredHabSpace, osDataSize, pgDataSize, err = ds.GetSpaceNeeded(ds.habDir)
	if err != nil {
		return err
	}
	res, err := ds.writer.Confirm(fmt.Sprintf("%d. "+MIN_REQ_SPACE_STR+". (You have current available space : %.1fGB)",
		*index, ds.habDir, ds.requiredHabSpace, ds.currentHabSpace))
	if err != nil {
		return status.Errorf(status.InvalidCommandArgsError, err.Error())
	}
	if !res {
		return status.New(status.InvalidCommandArgsError, fmt.Sprintf(diskSpaceError, ds.requiredHabSpace))
	}
	*index++
	if !ds.isExternalOS && ds.hasOSDestDir() {
		ds.requiredOSDestSpace = osDataSize
		ds.currentSpaceInOSDir, err = ds.fileutils.GetFreeSpaceinGB(ds.osDestDir)
		if err != nil {
			return err
		}
		res, err := ds.writer.Confirm(fmt.Sprintf("%d. "+MIN_REQ_SPACE_STR+". (You have current available space : %.1fGB)",
			*index, ds.osDestDir, ds.requiredOSDestSpace, ds.currentSpaceInOSDir))
		if err != nil {
			return status.Errorf(status.InvalidCommandArgsError, err.Error())
		}
		if !res {
			return status.New(status.InvalidCommandArgsError, fmt.Sprintf(diskSpaceError, ds.requiredOSDestSpace))
		}
		*index++
	}
	if !ds.isExternalPG {
		ds.requiredPGDestSpace = pgDataSize
		ds.currentSpaceInPGDir, err = ds.fileutils.GetFreeSpaceinGB(ds.pgDestDir)
		if err != nil {
			return err
		}
		res, err := ds.writer.Confirm(fmt.Sprintf("%d. "+MIN_REQ_SPACE_STR+". (You have current available space : %.1fGB)",
			*index, ds.pgDestDir, ds.requiredPGDestSpace, ds.currentSpaceInPGDir))
		if err != nil {
			return status.Errorf(status.InvalidCommandArgsError, err.Error())
		}
		if !res {
			return status.New(status.InvalidCommandArgsError, fmt.Sprintf(diskSpaceError, ds.requiredPGDestSpace))
		}
		*index++
	}
	return nil
}

func (ds *DiskSpaceInspection) hasOSDestDir() bool {
	if ds.osDestDir != "" && ds.osDestDir != HAB_DIR {
		return true
	}
	return false
}

func NewDiskSpaceInspection(w *cli.Writer, isExternalOS bool, isExternalPG bool, osDestDir string, fileUtils fileutils.FileUtils) *DiskSpaceInspection {
	return &DiskSpaceInspection{
		writer:       w,
		isExternalOS: isExternalOS,
		isExternalPG: isExternalPG,
		osDestDir:    osDestDir,
		pgDestDir:    fileutils.GetHabRootPath() + PG_DATA,
		fileutils:    fileUtils,
		checkDelay:   1000 * time.Millisecond,
	}
}

func (ds *DiskSpaceInspection) GetSpaceNeeded(habRootPath string) (habFreeSpace float64, osDataSize float64, pgDataSize float64, err error) {
	if ds.isExternalOS && ds.isExternalPG {
		return MIN_HAB_FREE_SPACE, -1, -1, nil
	} else if ds.isExternalOS && !ds.isExternalPG {
		pgDataPath := habRootPath + PG_DATA
		var rawPGDataSize float64
		rawPGDataSize, err = ds.fileutils.CalDirSizeInGB(pgDataPath)
		if err != nil {
			return -1, -1, -1, err
		}
		habFreeSpace = MIN_HAB_FREE_SPACE + (rawPGDataSize * 110 / 100)
		pgDataSize = (rawPGDataSize * 110 / 100)
		osDataSize = -1
		if ds.hasOSDestDir() {
			return MIN_HAB_FREE_SPACE, -1, pgDataSize, nil
		}
	} else if !ds.isExternalOS && ds.isExternalPG {
		osDataPath := habRootPath + OS_DATA
		var rawOSDataSize float64
		rawOSDataSize, err = ds.fileutils.CalDirSizeInGB(osDataPath)
		if err != nil {
			return -1, -1, -1, err
		}
		habFreeSpace = MIN_HAB_FREE_SPACE + (rawOSDataSize * 110 / 100)
		osDataSize = (rawOSDataSize * 110 / 100)
		pgDataSize = -1
		if ds.hasOSDestDir() {
			return MIN_HAB_FREE_SPACE, osDataSize, -1, nil
		}
	} else {
		pgDataPath := habRootPath + PG_DATA
		var rawPGDataSize float64
		rawPGDataSize, err = ds.fileutils.CalDirSizeInGB(pgDataPath)
		if err != nil {
			return -1, -1, -1, err
		}
		pgDataSize = (rawPGDataSize * 110 / 100)
		osDataPath := habRootPath + OS_DATA
		var rawOSDataSize float64
		rawOSDataSize, err = ds.fileutils.CalDirSizeInGB(osDataPath)
		if err != nil {
			return -1, -1, -1, err
		}
		habFreeSpace = MIN_HAB_FREE_SPACE + (rawOSDataSize * 110 / 100) + (rawPGDataSize * 110 / 100)
		osDataSize = (rawOSDataSize * 110 / 100)
		if ds.hasOSDestDir() {
			return MIN_HAB_FREE_SPACE, osDataSize, pgDataSize, nil
		}
	}
	return
}

func (ds *DiskSpaceInspection) showCheckingHab() {
	ds.spinnerHab = ds.writer.NewSpinner()
	ds.spinnerHab.Suffix = fmt.Sprintf("  [Checking]\t"+MIN_REQ_SPACE_STR, ds.habDir, ds.requiredHabSpace)
	ds.spinnerHab.Start()
	time.Sleep(ds.checkDelay)
}

func (ds *DiskSpaceInspection) showCheckingPGDest() {
	ds.spinnerPGDestDir = ds.writer.NewSpinner()
	ds.spinnerPGDestDir.Suffix = fmt.Sprintf("  [Checking]\t"+MIN_REQ_SPACE_STR, ds.pgDestDir, ds.requiredPGDestSpace)
	ds.spinnerPGDestDir.Start()
	time.Sleep(ds.checkDelay)
}

func (ds *DiskSpaceInspection) showCheckingOSDest() {
	if ds.hasOSDestDir() {
		ds.spinnerOSDestDir = ds.writer.NewSpinner()
		ds.spinnerOSDestDir.Suffix = fmt.Sprintf("  [Checking]\t"+MIN_REQ_SPACE_STR, ds.osDestDir, ds.requiredOSDestSpace)
		ds.spinnerOSDestDir.Start()
		time.Sleep(ds.checkDelay)
	}
}

func (ds *DiskSpaceInspection) checkHabSpace() bool {
	ds.showCheckingHab()
	if ds.currentHabSpace > ds.requiredHabSpace {
		ds.spinnerHab.FinalMSG = fmt.Sprintf(color.New(color.FgGreen).Sprint("✔")+"  ["+color.New(color.FgGreen).Sprint("Passed")+
			"]\t"+MIN_REQ_SPACE_STR+"\n", ds.habDir, ds.requiredHabSpace)
		ds.spinnerHab.Stop()
		return true
	} else {
		ds.spinnerHab.FinalMSG = fmt.Sprintf(color.New(color.FgRed).Sprint("✖")+"  ["+color.New(color.FgRed).Sprint("Failed")+
			"]\t"+MIN_REQ_SPACE_STR+"\n", ds.habDir, ds.requiredHabSpace)
		ds.spinnerHab.Stop()
		ds.skipOSDestCheck()
		return false
	}
}

func (ds *DiskSpaceInspection) checkPGDestSpace() bool {
	ds.showCheckingPGDest()
	if ds.currentSpaceInPGDir > ds.requiredPGDestSpace {
		ds.spinnerPGDestDir.FinalMSG = fmt.Sprintf(color.New(color.FgGreen).Sprint("✔")+"  ["+color.New(color.FgGreen).Sprint("Passed")+
			"]\t"+MIN_REQ_SPACE_STR+"\n", ds.pgDestDir, ds.requiredPGDestSpace)
		ds.spinnerPGDestDir.Stop()
		return true
	} else {
		ds.spinnerPGDestDir.FinalMSG = fmt.Sprintf(color.New(color.FgRed).Sprint("✖")+"  ["+color.New(color.FgRed).Sprint("Failed")+
			"]\t"+MIN_REQ_SPACE_STR+"\n", ds.pgDestDir, ds.requiredPGDestSpace)
		ds.spinnerPGDestDir.Stop()
		return false
	}
}

func (ds *DiskSpaceInspection) setErrHabSpace() {
	errStr := "[" + color.New(color.FgRed).Sprint("Error") + fmt.Sprintf("] Required Space : %.1fGB\n", ds.requiredHabSpace)
	errStr += fmt.Sprintf("        Available space : %.1fGB\n", ds.currentHabSpace)
	errStr += fmt.Sprintf(ENSURE_SPACE+color.New(color.Bold).Sprint(UPGRADE_CMD)+" command again", ds.requiredHabSpace)
	ds.exitError = errors.New(errStr)
	ds.exitedWithError = true
}

func (ds *DiskSpaceInspection) checkOSDestSpace() bool {
	ds.showCheckingOSDest()
	if ds.currentSpaceInOSDir > ds.requiredOSDestSpace {
		ds.spinnerOSDestDir.FinalMSG = fmt.Sprintf(color.New(color.FgGreen).Sprint("✔")+"  ["+color.New(color.FgGreen).Sprint("Passed")+
			"]\t"+MIN_REQ_SPACE_STR+"\n", ds.osDestDir, ds.requiredOSDestSpace)
		ds.spinnerOSDestDir.Stop()
		return true
	} else {
		ds.spinnerOSDestDir.FinalMSG = fmt.Sprintf(color.New(color.FgRed).Sprint("✖")+"  ["+color.New(color.FgRed).Sprint("Failed")+
			"]\t"+MIN_REQ_SPACE_STR+"\n", ds.osDestDir, ds.requiredOSDestSpace)
		ds.spinnerOSDestDir.Stop()
		return false
	}
}

func (ds *DiskSpaceInspection) setOSDestError() {
	errStr := "[" + color.New(color.FgRed).Sprint("Error") + fmt.Sprintf("] Required Space : %.1fGB\n", ds.requiredOSDestSpace)
	errStr += fmt.Sprintf("        Available space : %.1fGB\n", ds.currentSpaceInOSDir)
	errStr += fmt.Sprintf(ENSURE_SPACE+color.New(color.Bold).Sprint(UPGRADE_CMD)+" command again", ds.requiredOSDestSpace)
	ds.exitError = errors.New(errStr)
	ds.exitedWithError = true
}

func (ds *DiskSpaceInspection) setPGDestError() {
	errStr := "[" + color.New(color.FgRed).Sprint("Error") + fmt.Sprintf("] Required Space : %.1fGB\n", ds.requiredPGDestSpace)
	errStr += fmt.Sprintf("        Available space : %.1fGB\n", ds.currentSpaceInPGDir)
	errStr += fmt.Sprintf(ENSURE_SPACE+color.New(color.Bold).Sprint(UPGRADE_CMD)+" command again", ds.requiredPGDestSpace)
	ds.exitError = errors.New(errStr)
	ds.exitedWithError = true
}

func (ds *DiskSpaceInspection) skipOSDestCheck() {
	if ds.hasOSDestDir() {
		ds.writer.Printf(" ⊖  [Skipped]\t"+MIN_REQ_SPACE_STR+"\n", ds.osDestDir, ds.requiredOSDestSpace)
	}
}

func (ds *DiskSpaceInspection) Skip() {
	ds.writer.Printf(" ⊖  [Skipped]\t"+MIN_REQ_SPACE_STR+"\n", ds.habDir, ds.requiredHabSpace)
	ds.skipOSDestCheck()
}

func (ds *DiskSpaceInspection) Inspect() error {
	isHabFree := ds.checkHabSpace()
	if !isHabFree {
		ds.setErrHabSpace()
		return errors.New("failed in Hab Space Check")
	}
	if !ds.isExternalOS && ds.hasOSDestDir() {
		isOSDestFree := ds.checkOSDestSpace()
		if !isOSDestFree {
			ds.setOSDestError()
			return errors.New("failed in OS Dest Check")
		}
	}
	if !ds.isExternalPG {
		isPGDestFree := ds.checkPGDestSpace()
		if !isPGDestFree {
			ds.setPGDestError()
			return errors.New("failed in PG Dest Check")
		}
	}
	return nil
}

func (ds *DiskSpaceInspection) GetShortInfo() []string {
	msgs := []string{
		fmt.Sprintf(MIN_REQ_SPACE_STR, ds.habDir, ds.requiredHabSpace),
	}
	if !ds.isExternalOS && ds.hasOSDestDir() {
		msgs = append(msgs, fmt.Sprintf(MIN_REQ_SPACE_STR, ds.osDestDir, ds.requiredOSDestSpace))
	}
	if !ds.isExternalPG {
		msgs = append(msgs, fmt.Sprintf(MIN_REQ_SPACE_STR, ds.pgDestDir, ds.requiredPGDestSpace))
	}
	return msgs
}

func (ds *DiskSpaceInspection) GetInstallationType() inspector.InstallationType {
	return inspector.BOTH
}

func (ds *DiskSpaceInspection) ExitHandler() error {
	if ds.exitedWithError {
		ds.writer.Println(ds.exitError.Error())
	}
	return nil
}
