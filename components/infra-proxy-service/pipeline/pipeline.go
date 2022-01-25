package pipeline

import (
	"archive/zip"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strings"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/chef/automate/lib/logger"
)

func Unzip(src string) <-chan string {
	filePath := make(chan string)
	go func() {
		r, err := zip.OpenReader(src)
		if err != nil {
			logger.NewLogrusStandardLogger().Error("cannot open reader")
		}
		defer r.Close()

		for _, f := range r.File {
			fpath := filepath.Join("", f.Name)

			// Checking for any invalid file paths
			if !strings.HasPrefix(fpath, filepath.Clean("backup")+string(os.PathSeparator)) {
				logger.NewLogrusStandardLogger().Error("invalid path")
			}

			if f.FileInfo().IsDir() {
				os.MkdirAll(fpath, os.ModePerm)
				continue
			}

			// Creating the files in the target directory
			if err = os.MkdirAll(filepath.Dir(fpath), os.ModePerm); err != nil {
				logger.NewLogrusStandardLogger().Error("cannot create dir")
			}

			// The created file will be stored in
			// outFile with permissions to write &/or truncate
			outFile, err := os.OpenFile(fpath,
				os.O_WRONLY|os.O_CREATE|os.O_TRUNC,
				f.Mode())
			if err != nil {
				logger.NewLogrusStandardLogger().Error("cannot create a file")
			}

			rc, err := f.Open()
			if err != nil {
				logger.NewLogrusStandardLogger().Error("cannot open file")
			}

			_, err = io.Copy(outFile, rc)

			outFile.Close()
			rc.Close()

			if err != nil {
				logger.NewLogrusStandardLogger().Error("cannot copy a file")
			}
			filePath <- fpath
		}
		close(filePath)
	}()
	return filePath
}

func ParseKeyDump(fileDest <-chan string) <-chan storage.KeyDump {
	keyDump := make(chan storage.KeyDump)

	go func() {

		for input := range fileDest {
			file := path.Base(input)
			if file == "key_dump.json" {
				keyDumpByte, err := ioutil.ReadFile(input)
				if err != nil {
					logger.NewLogrusStandardLogger().Error("cannot read file")
				}
				var kd []storage.KeyDump
				if err := json.Unmarshal(keyDumpByte, &kd); err != nil {
					logger.NewLogrusStandardLogger().Error("cannot unmarshal sample1.json")
				}
				for _, k := range kd {
					keyDump <- k
				}

			}
		}

		close(keyDump)
	}()
	return keyDump
}

func ParseOrg(fileDest <-chan string) <-chan storage.Org {
	orgChan := make(chan storage.Org)

	go func() {

		for input := range fileDest {
			file := path.Base(input)
			// fmt.Println(file)
			if file == "org.json" {
				orgBytes, err := ioutil.ReadFile(input)
				if err != nil {
					logger.NewLogrusStandardLogger().Error("cannot read file")
				}
				var org storage.Org
				if err := json.Unmarshal(orgBytes, &org); err != nil {
					logger.NewLogrusStandardLogger().Error("cannot unmarshal sample1.json")
				}
				orgChan <- org
			}
		}

		close(orgChan)
	}()
	return orgChan
}

func SaveOrg(organizations <-chan storage.Org) <-chan string {
	result := make(chan string, 10)

	for org := range organizations {
		// Run SQL command to Save org
		result <- "saved org with id: " + org.ID
	}

	close(result)
	return result
}

func RunPipeline() {
	// Save Org
	data := SaveOrg(ParseOrg(Unzip("backup.zip")))
	for d := range data {
		fmt.Printf("Path: %+v\n", d)
	}
}
