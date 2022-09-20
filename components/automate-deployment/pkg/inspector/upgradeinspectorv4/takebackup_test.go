package upgradeinspectorv4

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
	"github.com/stretchr/testify/assert"
)

type TestWriter struct {
	WriteBuffer *bytes.Buffer
	ReadBuffer  *bytes.Buffer
	ErrorBuffer *bytes.Buffer
	CliWriter   *cli.Writer
}

func (tw *TestWriter) Input(userInput string) {
	tw.ReadBuffer.WriteString(fmt.Sprintf("%s\n", userInput))
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
	tw.SetCliWriter()
	return tw
}

func NewTestWriterWithInputs(inputs ...string) *TestWriter {
	tw := &TestWriter{
		WriteBuffer: new(bytes.Buffer),
		ReadBuffer:  new(bytes.Buffer),
		ErrorBuffer: new(bytes.Buffer),
	}
	for _, i := range inputs {
		tw.ReadBuffer.WriteString(fmt.Sprintf("%s\n", i))
	}
	tw.SetCliWriter()
	return tw
}

func (tw *TestWriter) SetCliWriter() {
	tw.CliWriter = cli.NewWriter(tw.WriteBuffer, tw.ErrorBuffer, tw.ReadBuffer)
}

func TestTakeBackup(t *testing.T) {
	tw := NewTestWriter()
	tb := NewTakeBackupInspection(tw.CliWriter)
	index := 2
	tb.ShowInfo(&index)
	expected := "2. You have taken backup by running command: " + color.New(color.Bold).Sprint("chef automate backup create\n")
	assert.Equal(t, expected, tw.Output())
}
