package main

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestGetAutomateHAInfraDetails(t *testing.T) {
	t.Run("Cannot read the file", func(t *testing.T) {
		actualInfraDetails, err := getAutomateHAInfraDetails("nonexistentfile.json")
		require.Error(t, err)
		require.Nil(t, actualInfraDetails)
	})

	t.Run("Cannot unmarshal", func(t *testing.T) {
		fileContent := `
		{
			"ssh_command": "ssh -p 22 user@host",
			"db_host": "localhost",
			"db_port": 3306,
			"db_user": "user",
			"db_password": "password",
		}
		`
		file, err := ioutil.TempFile("", "testfile*.json")
		require.NoError(t, err)

		defer os.Remove(file.Name())

		n, err := file.Write([]byte(fileContent))
		require.NoError(t, err)
		require.NotZero(t, n)

		_, err = getAutomateHAInfraDetails(file.Name())
		require.Error(t, err)
	})

	t.Run("passed all cases", func(t *testing.T) {
		// expectedInfraDetails := &AutomateHAInfraDetails{} // Define your expected infrastructure details here

		tmpfile, err := ioutil.TempFile("", "test-output.json")
		require.NoError(t, err)

		n, err := tmpfile.Write([]byte(`{"abc":"def"}`))
		require.NoError(t, err)
		require.NotZero(t, n)

		err = tmpfile.Close()
		require.NoError(t, err)

		actualInfraDetails, err := getAutomateHAInfraDetails(tmpfile.Name())
		require.NoError(t, err)
		require.NotNil(t, actualInfraDetails)

		err = os.Remove(tmpfile.Name())
		require.NoError(t, err)
	})
}
