// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestExecInfo(t *testing.T) {
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

func TestPrintInfo(t *testing.T) {

	t.Run("Passed ", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		var b bytes.Buffer
		infoCommandTemp = `AUTOMATION DETAILS:\n\tAutomate Admin User: \t\t \n\tAutomate Data Collector Token: \t \n\tAutomate Private IPs: \t\t\n\tAutomate SSH: \t\t\t\n\tAutomate URL: \t\t\t \nx\tBackup Config EFS: \t\t \n\tBackup Config S3: \t\t \n\tChef Server Private IPs: \t\n\tChef Server SSH: \t\t\n\tOpensearch Private IPs: \t\n\tOpensearch Public IPs: \t\t\n\tOpensearch SSH: \t\t\n\tPostgresql Private IPs: \t\n\tPostgresql SSH: \t\t\n\tSSH Key File: \t\t\t \n\tSSH Port: \t\t\t \n\tSSH User: \t\t\t \n\n\n`
		err := printInfo(infoCommandTemp, automate, &b)
		require.NoError(t, err)
		expected_string := `AUTOMATION DETAILS:\n\tAutomate Admin User: \t\t \n\tAutomate Data Collector Token: \t \n\tAutomate Private IPs: \t\t\n\tAutomate SSH: \t\t\t\n\tAutomate URL: \t\t\t \nx\tBackup Config EFS: \t\t \n\tBackup Config S3: \t\t \n\tChef Server Private IPs: \t\n\tChef Server SSH: \t\t\n\tOpensearch Private IPs: \t\n\tOpensearch Public IPs: \t\t\n\tOpensearch SSH: \t\t\n\tPostgresql Private IPs: \t\n\tPostgresql SSH: \t\t\n\tSSH Key File: \t\t\t \n\tSSH Port: \t\t\t \n\tSSH User: \t\t\t \n\n\n`
		require.Equal(t, expected_string, b.String())
	})

	t.Run("Failed to parse", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		var b bytes.Buffer
		infoCommandTemp = "Hello, {{Name}}"
		err := printInfo(infoCommandTemp, automate, &b)
		require.Error(t, err)
		require.Equal(t, "", b.String())

	})

	t.Run("Failed to execute tmpl", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		infoCommandTemp = "Hello, {{.Name}}"
		var b bytes.Buffer
		err := printInfo(infoCommandTemp, automate, &b)
		require.Error(t, err)
		require.Equal(t, "Hello, ", b.String())
	})
}
