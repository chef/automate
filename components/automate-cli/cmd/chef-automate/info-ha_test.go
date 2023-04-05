// Copyright © 2017 Chef Software

package main

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_execInfo(t *testing.T) {
	t.Run("FAIL: No file containg automate details ", func(t *testing.T) {
		automateHATerraformOutputFile = "file_not_found1.json"
		automateHATerraformDestroyOutputFile = "file_not_found1.json"
		err := execInfo()
		require.Error(t, err)

	})

	t.Run("FAIL: automate details don't exists ", func(t *testing.T) {
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

		automateHATerraformOutputFile = file.Name()
		automateHATerraformDestroyOutputFile = file.Name()
		err = execInfo()
		require.Error(t, err)

	})

	t.Run("PASS: automate details exist", func(t *testing.T) {
		fileContent := `
               {
               }
               `
		file, err := ioutil.TempFile("", "testfile*.json")
		require.NoError(t, err)

		defer os.Remove(file.Name())

		n, err := file.Write([]byte(fileContent))
		require.NoError(t, err)
		require.NotZero(t, n)

		automateHATerraformOutputFile = file.Name()
		automateHATerraformDestroyOutputFile = file.Name()
		err = execInfo()
		require.NoError(t, err)

	})

}

func Test_printInfo(t *testing.T) {
	t.Run("Passed ", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		tmpl, err := printInfo(automate)
		require.NoError(t, err)
		require.NotNil(t, tmpl)
	})

	t.Run("Failed to parse", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		infoCommandTemp = "Hello, {{Name}}"
		tmpl, err := printInfo(automate)
		require.Error(t, err)
		require.Nil(t, tmpl)
	})

	t.Run("Failed to execute tmpl", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		infoCommandTemp = "Hello, {{.Name}}"
		tmpl, err := printInfo(automate)
		require.Error(t, err)
		require.Nil(t, tmpl)
	})
}
