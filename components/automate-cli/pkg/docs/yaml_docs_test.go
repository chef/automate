package docs

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	yaml "gopkg.in/yaml.v2"
)

var _ = fmt.Sprintf("stop bothering me")

func newTmpDir() (string, func()) {
	tmpDir, _ := ioutil.TempDir("", "statusTmpDir")
	os.MkdirAll(tmpDir, os.ModePerm)
	return tmpDir, func() { os.RemoveAll(tmpDir) }
}

func TestStatusDocToYamlFile(t *testing.T) {
	t.Run("writes doc to YAML file", func(t *testing.T) {
		tmpDir, cleanup := newTmpDir()
		defer cleanup()

		doc := newStatusDoc()
		path := filepath.Join(tmpDir, "errors.yaml")
		require.NoError(t, doc.ToYamlFile(path))

		docFromFile := &statusDoc{}
		bytes, err := ioutil.ReadFile(path)
		require.NoError(t, err)
		require.NoError(t, yaml.Unmarshal(bytes, docFromFile))
		require.Equal(t, doc, docFromFile)
	})
}

func runExampleForTestCmd(cmd *cobra.Command, _ []string) error {
	return nil
}

func basicCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "example-for-test",
		RunE:  runExampleForTestCmd,
		Short: "just_for_testing_nothing_happens",
		Long:  "just_for_testing_nothing_happens",
	}
}

func parseResult(t *testing.T, tmpDir string) cmdDoc {
	resultPath := filepath.Join(tmpDir, "commands", "example-for-test.yaml")

	bytes, err := ioutil.ReadFile(resultPath)
	require.NoError(t, err)

	docFromFile := cmdDoc{}
	require.NoError(t, yaml.Unmarshal(bytes, &docFromFile))

	return docFromFile
}

func TestGenYamlTree(t *testing.T) {
	t.Run("creates some yaml for an empty command", func(t *testing.T) {
		tmpDir, cleanup := newTmpDir()
		defer cleanup()

		cmd := basicCmd()
		err := GenYamlTree(cmd, tmpDir)
		require.NoError(t, err)

		docFromFile := parseResult(t, tmpDir)
		// spot check instead of testing full equality to prevent whack-a-mode with
		// formatting of strings
		assert.Equal(t, "example-for-test", docFromFile.Name)
	})
	t.Run("includes flags in the yaml output", func(t *testing.T) {
		tmpDir, cleanup := newTmpDir()
		defer cleanup()

		cmd := basicCmd()

		var stringOpt string

		cmd.PersistentFlags().StringVar(&stringOpt,
			"string-opt", "default_value", "usage")
		err := GenYamlTree(cmd, tmpDir)
		require.NoError(t, err)

		docFromFile := parseResult(t, tmpDir)
		// spot check instead of testing full equality to prevent whack-a-mode with
		// formatting of strings
		assert.Equal(t, "example-for-test", docFromFile.Name)

		optDocumented := false
		var actualOpt cmdOption

		for _, opt := range docFromFile.Options {
			if opt.Name == "string-opt" {
				actualOpt = opt
				optDocumented = true
				break
			}
		}
		assert.True(t, optDocumented)
		assert.Equal(t, "default_value", actualOpt.DefaultValue)
	})
	t.Run("does not include hidden flags in the yaml output", func(t *testing.T) {
		tmpDir, cleanup := newTmpDir()
		defer cleanup()

		cmd := basicCmd()

		var stringOpt string

		cmd.PersistentFlags().StringVar(&stringOpt,
			"string-opt", "default_value", "usage")

		err := cmd.PersistentFlags().MarkHidden("string-opt")
		require.NoError(t, err)

		err = GenYamlTree(cmd, tmpDir)
		require.NoError(t, err)

		docFromFile := parseResult(t, tmpDir)
		// spot check instead of testing full equality to prevent whack-a-mode with
		// formatting of strings
		assert.Equal(t, "example-for-test", docFromFile.Name)

		optDocumented := false

		for _, opt := range docFromFile.Options {
			if opt.Name == "string-opt" {
				optDocumented = true
				break
			}
		}
		assert.False(t, optDocumented)
	})
}
