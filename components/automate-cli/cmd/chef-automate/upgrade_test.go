package main

import (
	"bytes"
	"path/filepath"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/stretchr/testify/assert"
)

type TestWriter struct {
	WriteBuffer *bytes.Buffer
	ReadBuffer  *bytes.Buffer
	ErrorBuffer *bytes.Buffer
	CliWriter   *cli.Writer
}

func (tw *TestWriter) Output() string {
	return tw.WriteBuffer.String()
}

func (tw *TestWriter) Error() string {
	return tw.ErrorBuffer.String()
}

func NewTestWriter() *TestWriter {
	tw := &TestWriter{
		WriteBuffer: new(bytes.Buffer),
		ReadBuffer:  new(bytes.Buffer),
		ErrorBuffer: new(bytes.Buffer),
	}
	tw.CliWriter = cli.NewWriter(tw.WriteBuffer, tw.ErrorBuffer, tw.ReadBuffer)
	return tw
}

func TestPrintAutomateOutOfDateWithMajor(t *testing.T) {
	tw := NewTestWriter()
	PrintAutomateOutOfDate(tw.CliWriter, "3.0.49", "4.2.59", true)
	expected := `Automate is out-of-date !!
Current version: 3.0.49
Latest upgradable version: 4.2.59

Please ensure you are using latest CLI version and then run the command:
  $ chef-automate upgrade run --major command to upgrade to latest version

Visit https://docs.chef.io/automate/major_upgrade_4.x for more information

`
	assert.Equal(t, expected, tw.Output())
}

func TestPrintAutomateOutOfDateWithoutMajor(t *testing.T) {
	tw := NewTestWriter()
	PrintAutomateOutOfDate(tw.CliWriter, "3.0.49", "3.0.59", false)
	expected := `Automate is out-of-date !!
Current version: 3.0.49
Latest upgradable version: 3.0.59

`
	assert.Equal(t, expected, tw.Output())
}

func TestPrintVersions(t *testing.T) {
	tw := NewTestWriter()
	PrintVersions(tw.CliWriter, "3.0.49", "4.2.59")
	expected := `Current version: 3.0.49
Target version: 4.2.59

`
	assert.Equal(t, expected, tw.Output())
}

func TestRemoveCommonContentFromAwsAutoTfvar(t *testing.T) {
	tempDir := t.TempDir()
	sourceFile := "../../pkg/testfiles/aws.auto.tfvars"
	destFile := filepath.Join(tempDir, "aws.auto.tfvars")
	copyFileContents(sourceFile, destFile)

	err := removeCommonContentFromAwsAutoTfvar(destFile)
	assert.NoError(t, err)
}
