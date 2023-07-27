package genconfig

import (
	"bytes"
	"errors"
	"io"
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/ioutils"
	"github.com/chef/automate/lib/pmt"
)

const esc = "\033["
const moveDown = esc + "1B"
const moveUp = esc + "1A"
const selectKey = "\r"
const clearLine = esc + "2K\n"

func TestGetAwsRegionSuccessCase(t *testing.T) {
	inputBytes := []byte(moveDown + moveDown + selectKey)
	in := io.NopCloser(bytes.NewBuffer(inputBytes))
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	mfsu := &fileutils.MockFileSystemUtils{}
	p := pmt.PromptFactory(in, out, mfsu)
	awsRegions := AwsRegionsImpFactory(p)
	val, err := awsRegions.Choose("AWS Region of bucket")

	assert.Equal(t, nil, err)
	assert.Equal(t, "us-west-1", val)
}

func TestGetAwsRegionErrorCase(t *testing.T) {
	inputBytes := []byte(moveDown + moveDown)
	in := io.NopCloser(bytes.NewBuffer(inputBytes))
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	mfsu := &fileutils.MockFileSystemUtils{}
	p := pmt.PromptFactory(in, out, mfsu)
	awsRegions := AwsRegionsImpFactory(p)
	val, err := awsRegions.Choose("AWS Region of bucket")

	assert.Equal(t, errors.New("prompt failed ^D"), err)
	assert.Equal(t, "", val)
}
