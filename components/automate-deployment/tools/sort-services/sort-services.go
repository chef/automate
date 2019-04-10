package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/chef/automate/components/automate-deployment/pkg/bind"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/services/parser"
	"github.com/chef/automate/lib/io/fileutils"
)

func fatal(msg string, err error) {
	fmt.Fprintf(os.Stderr, "%s: %s", msg, err.Error())
	os.Exit(1)
}

func main() {
	if len(os.Args) < 3 {
		fmt.Fprintln(os.Stderr, "usage: sort_services SERVICE_FILE_PATH BIND_FILE_PATH")
		os.Exit(1)
	}

	servicePath, err := filepath.Abs(os.Args[1])
	if err != nil {
		fatal("failed to expand service file path", err)
	}

	bindsPath, err := filepath.Abs(os.Args[2])
	if err != nil {
		fatal("failed to expand binds file path", err)
	}

	serviceData, err := ioutil.ReadFile(servicePath)
	if err != nil {
		fatal("failed to read service data", err)
	}

	collections, err := parser.ServiceCollectionsFromJSON(serviceData)
	if err != nil {
		fatal("failed to parse service data", err)
	}

	bindData, err := ioutil.ReadFile(bindsPath)
	if err != nil {
		fatal("failed to read bind data", err)
	}

	binds, err := bind.ParseData(bindData)
	if err != nil {
		fatal("failed to parse bind data", err)
	}

	for i, c := range collections {
		serviceList, err := habpkg.FromList(c.AllServices)
		if err != nil {
			fatal("failed to parse service data", err)
		}

		sorted, err := bind.TopoSortAll(serviceList, binds)
		if err != nil {
			fatal("failed to sort services", err)
		}

		sortedNames := make([]string, len(sorted))

		for j, pkg := range sorted {
			sortedNames[j] = habpkg.Ident(&pkg)
		}

		collections[i].AllServices = sortedNames
	}

	newContent := new(bytes.Buffer)
	encoder := json.NewEncoder(newContent)
	encoder.SetIndent("", "  ")

	err = encoder.Encode(collections)
	if err != nil {
		fatal("could not encode services list back to JSON", err)
	}

	err = fileutils.AtomicWrite(servicePath, newContent, fileutils.WithAtomicWriteNoSync(true))
	if err != nil {
		// Not cleaning up tempfile in this case, just in case
		fatal("failed to move new service file into place", err)
	}

	os.Exit(0)
}
