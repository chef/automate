package status

import (
	"encoding/json"
	"flag"
	"io/ioutil"
	"os"
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var update = flag.Bool("update", false, "update .golden files")

func TestNewCmdResult(t *testing.T) {
	t.Run("with no error", func(t *testing.T) {
		result := NewCmdResult(nil)
		assert.Equal(t, Ok, result.Status)
		assert.Equal(t, 0, result.ErrorCode)
		assert.Equal(t, "", result.ErrorDescription)
		assert.Equal(t, "", result.ErrorCause)
	})

	t.Run("with generic error", func(t *testing.T) {
		result := NewCmdResult(errors.New("generic error here"))
		assert.Equal(t, Failure, result.Status)
		assert.Equal(t, UnknownError, result.ErrorCode)
		assert.Equal(t, "generic error here", result.ErrorDescription)
		assert.Equal(t, "generic error here", result.ErrorCause)
	})

	t.Run("with status.Error", func(t *testing.T) {
		err := Wrap(errors.New("exec failed"), UpdateExecError, "execing the thing failed")
		result := NewCmdResult(err)
		assert.Equal(t, Failure, result.Status)
		assert.Equal(t, 112, result.ErrorCode)
		assert.Equal(t, "An issue occurred when trying to run an auto-updated CLI executable: execing the thing failed: exec failed",
			result.ErrorDescription)
		assert.Equal(t, "exec failed", result.ErrorCause)
	})
}

func TestCmdResultWriteFile(t *testing.T) {
	tmpFile, err := ioutil.TempFile("", t.Name())
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpFile.Name())

	result := &CmdResult{
		Command:          "chef-automate sub1 --be awesome",
		Status:           Failure,
		ErrorCode:        100,
		ErrorDescription: "a bad thing happened",
	}

	result.WriteFile(tmpFile.Name())

	gotBytes, err := ioutil.ReadFile(tmpFile.Name())
	if err != nil {
		t.Fatal(err)
	}

	newResult := &CmdResult{}
	err = json.Unmarshal(gotBytes, newResult)
	require.NoError(t, err)
	require.Equal(t, result, newResult)
}
