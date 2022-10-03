package upgradeinspectorv4

import (
	"errors"
	"fmt"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/fatih/color"
)

const (
	MIN_HAB_FREE_SPACE float64 = 5.5
	MIN_REQ_SPACE_STR          = "The %s/ directory should have at least %.1fGB of free space"
	ENSURE_SPACE               = "\nPlease ensure the available free space is %.1fGB\nand run "
	UPGRADE_CMD                = "chef-automate upgrade run --major"
	ES_DATA                    = "/svc/automate-elasticsearch/data"
)

type DiskSpaceInspection struct {
	writer              *cli.Writer
	isExternal          bool
	osDestDir           string
	fileutils           fileutils.FileUtils
	requiredHabSpace    float64
	requiredOSDestSpace float64
	currentHabSpace     float64
	currentSpaceInOSDir float64
	habDir              string
	spinnerHab          *spinner.Spinner
	spinnerOSDestDir    *spinner.Spinner
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
	var esDataSize float64
	ds.requiredHabSpace, esDataSize, err = ds.GetSpaceNeeded(ds.habDir)
	if err != nil {
		return err
	}
	ds.writer.Printf("%d. "+MIN_REQ_SPACE_STR+". (You have current available space : %.1fGB)\n",
		*index, ds.habDir, ds.requiredHabSpace, ds.currentHabSpace)
	*index++
	if !ds.isExternal && ds.hasOSDestDir() {
		ds.requiredOSDestSpace = esDataSize
		ds.currentSpaceInOSDir, err = ds.fileutils.GetFreeSpaceinGB(ds.osDestDir)
		if err != nil {
			return err
		}
		ds.writer.Printf("%d. "+MIN_REQ_SPACE_STR+". (You have current available space : %.1fGB)\n",
			*index, ds.osDestDir, ds.requiredOSDestSpace, ds.currentSpaceInOSDir)
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

func NewDiskSpaceInspection(w *cli.Writer, isExternal bool, osDestDir string, fileUtils fileutils.FileUtils) *DiskSpaceInspection {
	return &DiskSpaceInspection{
		writer:     w,
		isExternal: isExternal,
		osDestDir:  osDestDir,
		fileutils:  fileUtils,
		checkDelay: 1000 * time.Millisecond,
	}
}

func (ds *DiskSpaceInspection) GetSpaceNeeded(habRootPath string) (habFreeSpace float64, esDataSize float64, err error) {
	if ds.isExternal {
		return MIN_HAB_FREE_SPACE, -1, nil
	} else {
		esDataPath := habRootPath + ES_DATA
		var rawESDataSize float64
		rawESDataSize, err = ds.fileutils.CalDirSizeInGB(esDataPath)
		if err != nil {
			return -1, -1, err
		}
		habFreeSpace = MIN_HAB_FREE_SPACE + (rawESDataSize * 110 / 100)
		esDataSize = (rawESDataSize * 110 / 100)
		if ds.hasOSDestDir() {
			return MIN_HAB_FREE_SPACE, esDataSize, nil
		}
		return
	}
}

func (ds *DiskSpaceInspection) showCheckingHab() {
	ds.spinnerHab = ds.writer.NewSpinner()
	ds.spinnerHab.Suffix = fmt.Sprintf("  [Checking]\t"+MIN_REQ_SPACE_STR, ds.habDir, ds.requiredHabSpace)
	ds.spinnerHab.Start()
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
	if ds.hasOSDestDir() {
		isOSDestFree := ds.checkOSDestSpace()
		if !isOSDestFree {
			ds.setOSDestError()
			return errors.New("failed in OS Dest Check")
		}
	}
	return nil
}

func (ds *DiskSpaceInspection) GetShortInfo() []string {
	msgs := []string{
		fmt.Sprintf(MIN_REQ_SPACE_STR, ds.habDir, ds.requiredHabSpace),
	}
	if ds.hasOSDestDir() {
		msgs = append(msgs, fmt.Sprintf(MIN_REQ_SPACE_STR, ds.osDestDir, ds.requiredOSDestSpace))
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
