package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/sirupsen/logrus"
)

// This method reads all swagger .json files in the current folder
// and encodes them as strings literals in swagger.pb.go
func main() {
	files := map[string]os.FileInfo{}
	fail(filepath.Walk(".", func(path string, f os.FileInfo, err error) error {
		if strings.HasSuffix(f.Name(), ".json") {
			files[path] = f
		}
		return nil
	}))

	for path, f := range files {
		// iterate over all swagger defintions
		name := strings.Replace(f.Name(), ".swagger", "", -1)
		name = strings.TrimSuffix(name, ".json")
		prefix := strings.Replace(filepath.Dir(path), "/", "_", -1)
		outName := prefix + "_" + name + ".pb.swagger.go"
		logrus.Infof("Create file %s", outName)
		out, err := os.Create(outName)
		fail(err)
		f, err := os.Open(path)
		fail(err)

		reader := bufio.NewReader(f)
		writer := bufio.NewWriter(out)

		writer.WriteString("package api\n\nfunc init() {\n")
		writer.WriteString("	Swagger.Add(\"" + prefix + "_" + name + "\", `")

		for {
			line, isPrefix, err := reader.ReadLine()

			if line == nil {
				break
			}

			if isPrefix || err != nil {
				logrus.Fatalf("Failed with isPrefix=%t err=%v", isPrefix, err)
			}
			writer.WriteString(strings.Replace(string(line), "`", "` + \"`\" + `", -1))
			writer.WriteString("\n")
		}

		writer.WriteString("`)\n")
		writer.WriteString("}\n")
		writer.Flush()

		fail(f.Close())
		fail(out.Close())
	}
}

func fail(err error) {
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
