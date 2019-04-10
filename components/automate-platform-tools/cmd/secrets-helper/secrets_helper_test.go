package main_test

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"strconv"
	"strings"
	"sync/atomic"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/fileutils"
)

type testSecretsHelper struct {
	uniqInt  uint64
	tmpDir   string
	username string
}

func setupSecretsHelper(t *testing.T) *testSecretsHelper {
	h := &testSecretsHelper{}
	tmpDir, err := ioutil.TempDir("", "secrets-helper-test")
	if err != nil {
		t.Fatalf("Failed to create temp directory: %s", err.Error())
	}

	h.tmpDir = tmpDir
	cmd := exec.Command("go", "build", "-o", h.BinPath(), "./secrets-helper.go")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err = cmd.Run()
	if err != nil {
		t.Fatalf("Failed to build secrets-helper: %s", err.Error())
	}

	// A bit roundabout, but we get our UID, turn it into a
	// username, so in the test we can turn in back into a UID.
	user, err := user.LookupId(strconv.Itoa(os.Getuid()))
	if err != nil {
		t.Fatalf("Failed to look up running user: %s", err.Error())
	}

	h.username = user.Username
	return h
}

func (d *testSecretsHelper) DataDir() string {
	return filepath.Join(d.tmpDir, "data")
}

func (d *testSecretsHelper) BinPath() string {
	return filepath.Join(d.tmpDir, "secrets-helper")
}

func (d *testSecretsHelper) Cleanup() {
	os.RemoveAll(d.tmpDir)
}

func (d *testSecretsHelper) UniqueInt() uint64 {
	return atomic.AddUint64(&d.uniqInt, 1)
}

func (d *testSecretsHelper) CmdArgs(args []string) []string {
	cmdArgs := []string{
		args[0],
		"--data-dir", d.DataDir(),
		"--owner", d.username,
	}
	return append(cmdArgs, args[1:]...)
}

func (d *testSecretsHelper) RunSecretsHelper(args ...string) ([]byte, error) {
	cmd := exec.Command(d.BinPath(), d.CmdArgs(args)...)
	return cmd.CombinedOutput()
}

func (d *testSecretsHelper) RunSecretsHelperWithInput(input string, args ...string) ([]byte, error) {
	r := strings.NewReader(input)
	cmd := exec.Command(d.BinPath(), d.CmdArgs(args)...)
	cmd.Stdin = r
	return cmd.CombinedOutput()
}

func TestEndToEndSecretsHelper(t *testing.T) {
	helper := setupSecretsHelper(t)
	defer helper.Cleanup()

	t.Run("init creates the directory", func(t *testing.T) {
		_, err := helper.RunSecretsHelper("init")
		require.NoError(t, err)
		dirExists, _ := fileutils.PathExists(helper.DataDir())
		assert.True(t, dirExists)
	})

	t.Run("generates secrets of a given size", func(t *testing.T) {
		secretName := fmt.Sprintf("test.test-secret-%d", helper.UniqueInt())
		_, err := helper.RunSecretsHelper("generate", secretName, "16")
		require.NoError(t, err)

		output, err := helper.RunSecretsHelper("show", secretName)
		require.NoError(t, err)
		output = bytes.TrimRight(output, "\n")
		require.Equal(t, 16, len(output))

		// Test odd-value
		_, err = helper.RunSecretsHelper("generate", secretName, "15")
		require.NoError(t, err)

		output, err = helper.RunSecretsHelper("show", secretName)
		require.NoError(t, err)
		output = bytes.TrimRight(output, "\n")
		require.Equal(t, 15, len(output))

	})

	t.Run("DOES NOT overwrite a secret when --if-not-exists IS passed", func(t *testing.T) {
		secretName := fmt.Sprintf("test.test-secret-%d", helper.UniqueInt())
		_, err := helper.RunSecretsHelper("generate", secretName, "16", "--if-not-exists")
		require.NoError(t, err)

		firstSecret, err := helper.RunSecretsHelper("show", secretName)
		require.NoError(t, err)

		_, err = helper.RunSecretsHelper("generate", secretName, "16", "--if-not-exists")
		require.NoError(t, err)

		secondSecret, err := helper.RunSecretsHelper("show", secretName)
		require.NoError(t, err)
		assert.Equal(t, firstSecret, secondSecret)
	})

	t.Run("DOES overwrite a secret when --if-not-exists is NOT passed", func(t *testing.T) {
		secretName := fmt.Sprintf("test.test-secret-%d", helper.UniqueInt())
		_, err := helper.RunSecretsHelper("generate", secretName, "16")
		require.NoError(t, err)

		firstSecret, err := helper.RunSecretsHelper("show", secretName)
		require.NoError(t, err)

		_, err = helper.RunSecretsHelper("generate", secretName, "16")
		require.NoError(t, err)

		secondSecret, err := helper.RunSecretsHelper("show", secretName)
		require.NoError(t, err)
		assert.NotEqual(t, firstSecret, secondSecret)
	})

	t.Run("inserts a specific secret into the secret store", func(t *testing.T) {
		secretName := fmt.Sprintf("test.test-secret-%d", helper.UniqueInt())
		_, err := helper.RunSecretsHelperWithInput("some-specific-secret", "insert", secretName)
		require.NoError(t, err)

		secretValue, err := helper.RunSecretsHelper("show", secretName)
		secretValue = bytes.TrimRight(secretValue, "\n")
		require.NoError(t, err)
		assert.Equal(t, []byte("some-specific-secret"), secretValue)
	})

	t.Run("exec passes the requested secret to command it executes", func(t *testing.T) {
		secretName := fmt.Sprintf("test-secret-%d", helper.UniqueInt())
		fullName := fmt.Sprintf("test.%s", secretName)
		_, err := helper.RunSecretsHelperWithInput("some-specific-secret", "insert", fullName)
		require.NoError(t, err)

		output, err := helper.RunSecretsHelper("exec", "--secret", fullName, "--", "bash", "-c", "cat /dev/fd/$CHEF_SECRETS_FD")
		require.NoError(t, err)
		assert.Equal(t, []byte(fmt.Sprintf(`{"test":{"%s":"%s"}}`, secretName, "some-specific-secret")), output)
	})

	t.Run("exec fails if non-optional secret is missing", func(t *testing.T) {
		_, err := helper.RunSecretsHelper("exec", "--secret", "test.non-existing-secret", "--", "true")
		require.Error(t, err)
	})

	t.Run("exec passes if optional secret is missing", func(t *testing.T) {
		output, err := helper.RunSecretsHelper("exec", "--optional-secret", "test.non-existing-secret", "--", "bash", "-c", "cat /dev/fd/$CHEF_SECRETS_FD")
		require.NoError(t, err)
		// Get the last two bits of output to avoid parsing warning message
		assert.Equal(t, []byte(`{}`), output[len(output)-2:])
	})

	t.Run("exec passes the requested optional secret to command it executes", func(t *testing.T) {
		secretName := fmt.Sprintf("test-secret-%d", helper.UniqueInt())
		fullName := fmt.Sprintf("test.%s", secretName)
		_, err := helper.RunSecretsHelperWithInput("some-specific-secret", "insert", fullName)
		require.NoError(t, err)

		output, err := helper.RunSecretsHelper("exec", "--optional-secret", fullName, "--", "bash", "-c", "cat /dev/fd/$CHEF_SECRETS_FD")
		require.NoError(t, err)
		assert.Equal(t, []byte(fmt.Sprintf(`{"test":{"%s":"%s"}}`, secretName, "some-specific-secret")), output)
	})

}
