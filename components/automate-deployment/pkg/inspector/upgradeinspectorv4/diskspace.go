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
	ds.writer.Printf("%d. %s directory should have %.1fGB of free space. (Currently available space : %.1fGB)\n",
		*index, ds.habDir, ds.requiredHabSpace, ds.currentHabSpace)
	if !ds.isExternal && ds.hasOSDestDir() {
		*index++
		ds.requiredOSDestSpace = esDataSize
		ds.currentSpaceInOSDir, err = ds.fileutils.GetFreeSpaceinGB(ds.osDestDir)
		if err != nil {
			return err
		}
		ds.writer.Printf("%d. %s directory should have %.1fGB of free space. (Currently available space : %.1fGB)\n",
			*index, ds.osDestDir, ds.requiredOSDestSpace, ds.currentSpaceInOSDir)
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
		esDataPath := habRootPath + "/svc/automate-elasticsearch/data"
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
	ds.spinnerHab.Suffix = fmt.Sprintf("  [Checking]\t%s directory should have %.1fGB of free space", ds.habDir, ds.requiredHabSpace)
	ds.spinnerHab.Start()
	time.Sleep(ds.checkDelay)
}

func (ds *DiskSpaceInspection) showCheckingOSDest() {
	if ds.hasOSDestDir() {
		ds.spinnerOSDestDir = ds.writer.NewSpinner()
		ds.spinnerOSDestDir.Suffix = fmt.Sprintf("  [Checking]\t%s directory should have %.1fGB of free space", ds.osDestDir, ds.requiredOSDestSpace)
		ds.spinnerOSDestDir.Start()
		time.Sleep(ds.checkDelay)
	}
}

func (ds *DiskSpaceInspection) checkHabSpace() bool {
	ds.showCheckingHab()
	if ds.currentHabSpace > ds.requiredHabSpace {
		ds.spinnerHab.FinalMSG = fmt.Sprintf(color.New(color.FgGreen).Sprint("✔")+"  ["+color.New(color.FgGreen).Sprint("Passed")+
			"]\t%s directory should have %.1fGB of free space\n", ds.habDir, ds.requiredHabSpace)
		ds.spinnerHab.Stop()
		return true
	} else {
		ds.spinnerHab.FinalMSG = fmt.Sprintf(color.New(color.FgRed).Sprint("✖")+"  ["+color.New(color.FgRed).Sprint("Failed")+
			"]\t%s directory should have %.1fGB of free space\n", ds.habDir, ds.requiredHabSpace)
		ds.spinnerHab.Stop()
		ds.skipOSDestCheck()
		ds.showErrHabSpace()
		return false
	}
}

func (ds *DiskSpaceInspection) showErrHabSpace() {
	ds.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + fmt.Sprintf("] Required Space : %.1fGB", ds.requiredHabSpace))
	ds.writer.Printf("        Available space : %.1fGB\n", ds.currentHabSpace)
	ds.writer.Printf("\nPlease ensure the available free space is %.1fGB\nand run "+color.New(color.Bold).Sprint("chef-automate upgrade run --major")+" command again", ds.requiredHabSpace)
	ds.writer.BufferWriter().Flush()
}

func (ds *DiskSpaceInspection) checkOSDestSpace() bool {
	ds.showCheckingOSDest()
	if ds.currentSpaceInOSDir > ds.requiredOSDestSpace {
		ds.spinnerOSDestDir.FinalMSG = fmt.Sprintf(color.New(color.FgGreen).Sprint("✔")+"  ["+color.New(color.FgGreen).Sprint("Passed")+
			"]\t%s directory should have %.1fGB of free space\n", ds.osDestDir, ds.requiredOSDestSpace)
		ds.spinnerOSDestDir.Stop()
		return true
	} else {
		ds.spinnerOSDestDir.FinalMSG = fmt.Sprintf(color.New(color.FgRed).Sprint("✖")+"  ["+color.New(color.FgRed).Sprint("Failed")+
			"]\t%s directory should have %.1fGB of free space\n", ds.osDestDir, ds.requiredOSDestSpace)
		ds.spinnerOSDestDir.Stop()
		ds.showOSDestError()
		return false
	}
}

func (ds *DiskSpaceInspection) showOSDestError() {
	ds.writer.Println("[" + color.New(color.FgRed).Sprint("Error") + fmt.Sprintf("] Required Space : %.1fGB", ds.requiredOSDestSpace))
	ds.writer.Printf("        Available space : %.1fGB\n", ds.currentSpaceInOSDir)
	ds.writer.Printf("\nPlease ensure the available free space is %.1fGB\nand run "+color.New(color.Bold).Sprint("chef-automate upgrade run --major")+" command again", ds.requiredOSDestSpace)
	ds.writer.BufferWriter().Flush()
}

func (ds *DiskSpaceInspection) skipOSDestCheck() {
	ds.writer.Printf(" ⊖  [Skipped]\t%s directory should have %.1fGB of free space\n\n", ds.osDestDir, ds.requiredOSDestSpace)
}

func (ds *DiskSpaceInspection) Skip() {
	ds.writer.Printf(" ⊖  [Skipped]\t%s directory should have %.1fGB of free space\n\n", ds.habDir, ds.requiredHabSpace)
	ds.writer.Printf(" ⊖  [Skipped]\t%s directory should have %.1fGB of free space\n\n", ds.osDestDir, ds.requiredOSDestSpace)
}

func (ds *DiskSpaceInspection) Inspect() error {
	isHabFree := ds.checkHabSpace()
	if !isHabFree {
		return errors.New("failed in Hab Space Check")
	}
	if ds.hasOSDestDir() {
		isOSDestFree := ds.checkOSDestSpace()
		if !isOSDestFree {
			return errors.New("failed in OS Dest Check")
		}
	}
	return nil
}

func (ds *DiskSpaceInspection) GetShortInfo() []string {
	msgs := []string{
		fmt.Sprintf("%s directory should have %.1fGB of free space", ds.habDir, ds.requiredHabSpace),
	}
	if ds.hasOSDestDir() {
		msgs = append(msgs, fmt.Sprintf("%s directory should have %.1fGB of free space", ds.osDestDir, ds.requiredOSDestSpace))
	}
	return msgs
}

func (ds *DiskSpaceInspection) GetInstallationType() inspector.InstallationType {
	return inspector.BOTH
}
