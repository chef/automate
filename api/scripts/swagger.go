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
	fail(filepath.Walk("./external", func(path string, f os.FileInfo, err error) error {
		if strings.HasSuffix(f.Name(), ".json") {
			files[path] = f
		}
		return nil
	}))

	for path, f := range files {
		// iterate over all swagger defintions
		if strings.HasSuffix(f.Name(), ".json") {
			name := strings.Replace(f.Name(), ".swagger", "", -1)
			name = strings.TrimSuffix(name, ".json")
			outName := "../components/automate-gateway/api/" + name + ".pb.swagger.go"
			logrus.Infof("Create file %s", outName)
			out, err := os.Create(outName)
			fail(err)
			f, err := os.Open(path)
			fail(err)

			reader := bufio.NewReader(f)
			writer := bufio.NewWriter(out)

			_, _ = writer.WriteString("package api\n\nfunc init() {\n")   // nolint: errcheck
			_, _ = writer.WriteString("	Swagger.Add(\"" + name + "\", `") // nolint: errcheck

			for {
				line, is_prefix, err := reader.ReadLine()

				if line == nil {
					break
				}

				if is_prefix || err != nil {
					logrus.Fatalf("Failed with is_prefix=%t err=%v", is_prefix, err)
				}
				_, _ = writer.WriteString(strings.Replace(string(line), "`", "` + \"`\" + `", -1))
				_, _ = writer.WriteString("\n")
			}

			_, _ = writer.WriteString("`)\n")
			_, _ = writer.WriteString("}\n")
			writer.Flush()

			f.Close()
			out.Close()
		}
	}
}

func fail(err error) {
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
