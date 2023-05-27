package hardwarecal

import (
	"bytes"
	"errors"
	"io"
	"os"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/ioutils"
	"github.com/chef/automate/lib/pmt"
	"github.com/stretchr/testify/assert"
)

const esc = "\033["
const moveDown = esc + "1B"
const moveUp = esc + "1A"
const selectKey = "\r"
const clearLine = esc + "2K\n"

func pad(siz int, buf *bytes.Buffer) {
	pu := make([]byte, 4096-siz)
	for i := 0; i < 4096-siz; i++ {
		pu[i] = 97
	}
	buf.Write(pu)
}

func input(buf *bytes.Buffer, input string) {
	buf.WriteString(input)
	pad(len(input), buf)
}

func TestHardwareCalPromptRun(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, "\r")
	input(b, "\r")
	input(b, "\r")
	input(b, "\r")
	input(b, "\r")
	input(b, "\r")
	input(b, "\r")
	input(b, "\r")
	input(b, "\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == "test.pem" {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := pmt.PromptFactory(in, out, mfsu)

	cwIn := &bytes.Buffer{}
	cwOut := &bytes.Buffer{}
	cw := cli.NewWriter(cwOut, cwOut, cwIn)

	hp := &HardwareCalPrompt{
		Prompt: p,
		cw:     cw,
	}
	err := hp.Run()

	assert.NoError(t, err)
	output := cwOut.String()
	expetedOutput := `Hardware Requirements
+-------------------+-------+------+----------+--------------------+------------------+
| INSTANCE          | COUNT | VCPU | RAM (GB) | STORAGE SPACE (GB) | AWS MACHINE TYPE |
+-------------------+-------+------+----------+--------------------+------------------+
| Chef Automate     |     2 |    2 |        8 |                200 | m5.large         |
| Chef Infra Server |     2 |    2 |        8 |                200 | m5.large         |
| PostgreSQL        |     3 |    2 |        8 |                200 | m5.large         |
| OpenSearch        |     3 |    2 |        8 |               1300 | m5.large         |
+-------------------+-------+------+----------+--------------------+------------------+
`
	assert.Equal(t, expetedOutput, output)
}
