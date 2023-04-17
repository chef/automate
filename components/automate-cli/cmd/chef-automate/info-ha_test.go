// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/require"
)

var (
	testSSHInfoCmd = "ssh -i file.pem -p 22 user@ip"
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

func createInputDetails() *AutomateHAInfraDetails {

	automate := &AutomateHAInfraDetails{}
	automate.Outputs.AutomateAdminPassword.Value = "testpassowrd"
	automate.Outputs.AutomateAdminUser.Value = "admin"
	automate.Outputs.AutomateDataCollectorToken.Value = "token"
	automate.Outputs.AutomateFrontendUrls.Value = "https://<front-end>"
	automate.Outputs.AutomatePrivateIps.Value = []string{"host1", "host2"}
	automate.Outputs.AutomateSSH.Value = []string{testSSHInfoCmd, testSSHInfoCmd}
	automate.Outputs.AutomateURL.Value = "https://example.com"
	automate.Outputs.ChefServerPrivateIps.Value = []string{"host1", "host2"}
	automate.Outputs.ChefServerSSH.Value = []string{testSSHInfoCmd, testSSHInfoCmd}
	automate.Outputs.OpensearchPrivateIps.Value = []string{"host1", "host2", "host3"}
	automate.Outputs.OpensearchSSH.Value = []string{testSSHInfoCmd, testSSHInfoCmd, testSSHInfoCmd}
	automate.Outputs.PostgresqlPrivateIps.Value = []string{"host1", "host2", "host3"}
	automate.Outputs.PostgresqlSSH.Value = []string{testSSHInfoCmd, testSSHInfoCmd, testSSHInfoCmd}
	automate.Outputs.SSHKeyFile.Value = "/path/to/key"
	automate.Outputs.SSHPort.Value = "port"
	automate.Outputs.SSHUser.Value = "user"

	return automate

}

var expectedAutomateDetails = `AUTOMATE DETAILS:
                             automate_admin_user: admin
                   automate_data_collector_token: token
                          automate_frontend_urls: https://example.com
                            automate_private_ips: host1
                                                  host2
                                    automate_ssh: ssh -i file.pem -p 22 user@ip
                                                  ssh -i file.pem -p 22 user@ip
                                    automate_url: https://example.com
                         chef_server_private_ips: host1
                                                  host2
                                 chef_server_ssh: ssh -i file.pem -p 22 user@ip
                                                  ssh -i file.pem -p 22 user@ip
                          opensearch_private_ips: host1
                                                  host2
                                                  host3
                                  opensearch_ssh: ssh -i file.pem -p 22 user@ip
                                                  ssh -i file.pem -p 22 user@ip
                                                  ssh -i file.pem -p 22 user@ip
                          postgresql_private_ips: host1
                                                  host2
                                                  host3
                                  postgresql_ssh: ssh -i file.pem -p 22 user@ip
                                                  ssh -i file.pem -p 22 user@ip
                                                  ssh -i file.pem -p 22 user@ip
                                    ssh_key_file: /path/to/key
                                        ssh_port: port
                                        ssh_user: user

`

func TestPrintInfo(t *testing.T) {

	t.Run("Checking valid result ", func(t *testing.T) {
		automate := createInputDetails()
		var b bytes.Buffer
		err := printInfo(infoCommandTemp, automate, &b)
		require.NoError(t, err)

		require.Equal(t, expectedAutomateDetails, b.String())
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
