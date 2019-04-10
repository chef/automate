package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"strings"

	"github.com/chef/automate/components/automate-deployment/pkg/a1upgrade"
)

func main() {
	fmt.Fprintf(os.Stderr, "pg_dump test stub called as: %s\n", strings.Join(os.Args, " "))
	if a1upgrade.Failure(a1upgrade.ExtractA1DataFail) {
		fmt.Fprintln(os.Stderr, "pg_dump FAILURE (simulated)")
		os.Exit(1)
	}

	outputPath := path.Join(a1upgrade.PGDumpOutputDir, fmt.Sprintf("%s.sql", os.Args[len(os.Args)-1]))
	for i, arg := range os.Args {
		if arg == "--file" {
			outputPath = os.Args[i+1]
			break
		}
	}
	_, err := os.Stat(outputPath)
	if os.IsNotExist(err) {
		err := ioutil.WriteFile(outputPath, []byte(defaultPGDumpContents), 0600)
		if err != nil {
			fmt.Fprintf(os.Stderr, "pg_dump failed to write fake output: %s\n", err.Error())
			os.Exit(1)
		}
	} else {
		fmt.Fprintln(os.Stderr, "pg_dump output data already exists.  Assuming this is from an a1-migration-data package.")
	}

	fmt.Fprintln(os.Stderr, "pg_dump SUCCESS (simulated)")
	os.Exit(0)
}
