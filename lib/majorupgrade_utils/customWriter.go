package majorupgrade_utils

import (
	"bytes"
	"fmt"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type CustomWriter struct {
	WriteBuffer *bytes.Buffer
	ReadBuffer  *bytes.Buffer
	ErrorBuffer *bytes.Buffer
	CliWriter   *cli.Writer
}

func (cw *CustomWriter) Input(userInput string) {
	cw.ReadBuffer.WriteString(fmt.Sprintf("%s\n", userInput))
}

func (cw *CustomWriter) Output() string {
	return cw.WriteBuffer.String()
}

func (cw *CustomWriter) Error() string {
	return cw.ErrorBuffer.String()
}

func NewCustomWriter() *CustomWriter {
	cw := &CustomWriter{
		WriteBuffer: new(bytes.Buffer),
		ReadBuffer:  new(bytes.Buffer),
		ErrorBuffer: new(bytes.Buffer),
	}
	cw.SetCliWriter()
	return cw
}

func NewCustomWriterWithInputs(inputs ...string) *CustomWriter {
	cw := &CustomWriter{
		WriteBuffer: new(bytes.Buffer),
		ReadBuffer:  new(bytes.Buffer),
		ErrorBuffer: new(bytes.Buffer),
	}
	for _, i := range inputs {
		cw.ReadBuffer.WriteString(fmt.Sprintf("%s\n", i))
	}
	cw.SetCliWriter()
	return cw
}

func (cw *CustomWriter) SetCliWriter() {
	cw.CliWriter = cli.NewWriter(cw.WriteBuffer, cw.ErrorBuffer, cw.ReadBuffer)
}
