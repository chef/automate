package main

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var (
	testFile            = "testfile*.json"
	nonsxistingtfstate2 = "nonsxisting2.tfstate"
	testSSHCmd1         = "ssh -i path/secret.pem -p 22 ubuntu@127.0.0.1"
)

func TestGetAutomateHAInfraDetailsHelper(t *testing.T) {
	// Valid json
	t.Run("passed all cases", func(t *testing.T) {

		fileContent := `
       {
           "outputs": {
             "automate_admin_password": {
               "value": "abcdef",
               "type": "string"
             },
             "automate_admin_user": {
               "value": "admin",
               "type": "string"
             },
             "automate_data_collector_token": {
               "value": "sampletokendata",
               "type": "string"
             },
             "automate_private_ips": {
               "value": [
                 "127.0.0.1",
                 "127.0.0.1"
               ],
               "type": [
                 "list",
                 "string"
               ]
             },
             "automate_ssh": {
               "value": [
                 "ssh -i path/secret.pem -p 22 ubuntu@127.0.0.1",
                 "ssh -i path/secret.pem -p 22 ubuntu@127.0.0.1"
               ],
               "type": [
                 "list",
                 "string"
               ]
             },
             "automate_url": {
               "value": "https://example.com",
               "type": "string"
             },
             "backup_config_efs": {
               "value": "true",
               "type": "string"
             },
             "backup_config_s3": {
               "value": "false",
               "type": "string"
             }
           }
         }`

		tmpfile, err := ioutil.TempFile("", "test-output.json")
		require.NoError(t, err)

		n, err := tmpfile.Write([]byte(fileContent))
		require.NoError(t, err)
		require.NotZero(t, n)

		err = tmpfile.Close()
		require.NoError(t, err)

		actualInfraDetails, err := getAutomateHAInfraDetailHelper(tmpfile.Name())
		require.NoError(t, err)
		require.NotNil(t, actualInfraDetails)
		require.Equal(t, actualInfraDetails.Outputs.AutomateAdminUser.Value, "admin")

		err = os.Remove(tmpfile.Name())
		require.NoError(t, err)
	})

	// Test case 2: Invalid JSON file
	t.Run("Invalid JSON file", func(t *testing.T) {
		fileContent := `
               {
                   "ssh_command": "ssh -p 22 user@host",
                   "db_host": "localhost",
                   "db_port": 3306,
                   "db_user": "user",
                   "db_password": "password",
               }
               `
		file, err := ioutil.TempFile("", testFile)
		require.NoError(t, err)

		defer os.Remove(file.Name())

		n, err := file.Write([]byte(fileContent))
		require.NoError(t, err)
		require.NotZero(t, n)

		_, err = getAutomateHAInfraDetailHelper(file.Name())
		require.Error(t, err)
	})

	// Test case 3: File not found
	t.Run("File not found", func(t *testing.T) {

		filePath := "testdata/notfound.json"
		_, err := getAutomateHAInfraDetailHelper(filePath)
		require.Error(t, err)
	})

	// Test case 4: Empty file
	t.Run("Empty file", func(t *testing.T) {
		file, err := ioutil.TempFile("", testFile)
		require.NoError(t, err)

		_, err = getAutomateHAInfraDetailHelper(file.Name())
		require.Error(t, err)
		require.Equal(t, err.Error(), "the file is empty")
	})
}

func TestExtractPortAndSshUserFromAutomateSSHCommand(t *testing.T) {
	t.Run("AutomateInfraDetails is nil", func(t *testing.T) {
		automateHAInfraDetails := &AutomateHAInfraDetails{}
		extractPortAndSshUserFromAutomateSSHCommand(nil)
		assert.Empty(t, automateHAInfraDetails.Outputs.SSHPort.Value)
		assert.Empty(t, automateHAInfraDetails.Outputs.SSHUser.Value)
	})

	t.Run("SSH details available", func(t *testing.T) {
		automateHAInfraDetails := &AutomateHAInfraDetails{}
		automateHAInfraDetails.Outputs.AutomateSSH.Value = []string{testSSHCmd1}
		automateHAInfraDetails.Outputs.SSHPort.Value = "443"
		automateHAInfraDetails.Outputs.SSHUser.Value = "ubuntu1234"
		extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails)
		assert.Equal(t, "443", automateHAInfraDetails.Outputs.SSHPort.Value)
		assert.Equal(t, "ubuntu1234", automateHAInfraDetails.Outputs.SSHUser.Value)
	})

	t.Run("Empty SSH user", func(t *testing.T) {
		automateHAInfraDetails := &AutomateHAInfraDetails{}
		automateHAInfraDetails.Outputs.AutomateSSH.Value = []string{testSSHCmd1}
		automateHAInfraDetails.Outputs.SSHPort.Value = "443"
		automateHAInfraDetails.Outputs.SSHUser.Value = ""
		extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails)
		assert.Equal(t, "443", automateHAInfraDetails.Outputs.SSHPort.Value)
		assert.Equal(t, "ubuntu", automateHAInfraDetails.Outputs.SSHUser.Value)
	})

	t.Run("Empty SSH port", func(t *testing.T) {
		automateHAInfraDetails := &AutomateHAInfraDetails{}
		automateHAInfraDetails.Outputs.AutomateSSH.Value = []string{testSSHCmd1}
		automateHAInfraDetails.Outputs.SSHPort.Value = ""
		automateHAInfraDetails.Outputs.SSHUser.Value = "user234"
		extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails)
		assert.Equal(t, "22", automateHAInfraDetails.Outputs.SSHPort.Value)
		assert.Equal(t, "user234", automateHAInfraDetails.Outputs.SSHUser.Value)
	})

	t.Run("No SSH port in SSH details", func(t *testing.T) {
		automateHAInfraDetails := &AutomateHAInfraDetails{}
		automateHAInfraDetails.Outputs.AutomateSSH.Value = []string{"ssh -i /path/to/key.pem user@hostname"}
		extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails)
		assert.Equal(t, "22", automateHAInfraDetails.Outputs.SSHPort.Value)
		assert.Equal(t, "user", automateHAInfraDetails.Outputs.SSHUser.Value)
	})

	t.Run("SSH port in SSH details", func(t *testing.T) {
		automateHAInfraDetails := &AutomateHAInfraDetails{}
		automateHAInfraDetails.Outputs.AutomateSSH.Value = []string{"ssh -i /path/to/key.pem -p 23 user@hostname"}
		extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails)
		assert.Equal(t, "23", automateHAInfraDetails.Outputs.SSHPort.Value)
		assert.Equal(t, "user", automateHAInfraDetails.Outputs.SSHUser.Value)
	})

	t.Run("SSH port in SSH details", func(t *testing.T) {
		automateHAInfraDetails := &AutomateHAInfraDetails{}
		automateHAInfraDetails.Outputs.AutomateSSH.Value = []string{"ssh -i /path/to/key.pem -p 2222 user@hostname"}
		automateHAInfraDetails.Outputs.SSHPort.Value = "2345"
		extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails)
		assert.Equal(t, "2345", automateHAInfraDetails.Outputs.SSHPort.Value)
		assert.Equal(t, "user", automateHAInfraDetails.Outputs.SSHUser.Value)
	})

	t.Run("Default SSH port", func(t *testing.T) {
		automateHAInfraDetails := &AutomateHAInfraDetails{}
		automateHAInfraDetails.Outputs.AutomateSSH.Value = []string{"ssh -i /path/to/key.pem user@hostname"}
		automateHAInfraDetails.Outputs.SSHPort.Value = ""
		extractPortAndSshUserFromAutomateSSHCommand(automateHAInfraDetails)
		assert.Equal(t, "22", automateHAInfraDetails.Outputs.SSHPort.Value)
		assert.Equal(t, "user", automateHAInfraDetails.Outputs.SSHUser.Value)
	})
}

func TestFileContainingAutomateHAInfraDetails(t *testing.T) {
	t.Run("Failed", func(t *testing.T) {
		automateHATerraformOutputFile = "nonsxisting1.tfstate"
		automateHATerraformDestroyOutputFile = nonsxistingtfstate2

		str, err := FileContainingAutomateHAInfraDetails()
		require.Error(t, err)
		require.Empty(t, str)
	})

	t.Run("return first file", func(t *testing.T) {
		file, err := ioutil.TempFile("", testFile)
		require.NoError(t, err)

		defer os.Remove(file.Name())
		automateHATerraformOutputFile = file.Name()
		automateHATerraformDestroyOutputFile = nonsxistingtfstate2

		str, err := FileContainingAutomateHAInfraDetails()
		require.NoError(t, err)
		require.Equal(t, str, file.Name())
	})

	t.Run("return second file", func(t *testing.T) {
		file, err := ioutil.TempFile("", testFile)
		require.NoError(t, err)

		defer os.Remove(file.Name())
		automateHATerraformOutputFile = nonsxistingtfstate2
		automateHATerraformDestroyOutputFile = file.Name()

		str, err := FileContainingAutomateHAInfraDetails()
		require.NoError(t, err)
		require.Equal(t, str, file.Name())
	})
}
