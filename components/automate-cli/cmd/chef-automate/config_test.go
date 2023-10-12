package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"testing"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var fqdn = "a2.test.com"

func TestIsConfigChanged(t *testing.T) {

	t.Run("check if the values are changed For Postgresql", func(t *testing.T) {
		reqConfig := PostgresqlConfig{
			Host: fqdn,
			Port: 80,
		}

		existingConfig := PostgresqlConfig{
			Host: "a5.test.com",
			Port: 22,
		}

		isChanged := isConfigChanged(existingConfig, reqConfig)
		assert.True(t, isChanged)

	})
	t.Run("check if the some values are added For Postgresql", func(t *testing.T) {
		reqConfig := PostgresqlConfig{
			Host:              fqdn,
			Port:              80,
			CheckpointTimeout: "10c",
		}

		existingConfig := PostgresqlConfig{
			Host: "a5.test.com",
		}

		isChanged := isConfigChanged(existingConfig, reqConfig)
		assert.True(t, isChanged)

	})
	t.Run("check if the no values are added or changed", func(t *testing.T) {
		reqConfig := PostgresqlConfig{
			Host:              fqdn,
			Port:              80,
			CheckpointTimeout: "10c",
		}

		existingConfig := PostgresqlConfig{
			Host:              fqdn,
			Port:              80,
			CheckpointTimeout: "10c",
		}

		isChanged := isConfigChanged(existingConfig, reqConfig)
		assert.False(t, isChanged)

	})

}

func TestPostgresSqlDecodeFromInput(t *testing.T) {
	t.Run("If there are some parameters", func(t *testing.T) {
		req := `[pg_dump]
enable = true
path = "/mnt/automate_backups/postgresql/pg_dump"
[replication]
lag_health_threshold = 20480
max_replay_lag_before_restart_s = 180
name = "replication"
password = "replication"`

		config, _ := getDecodedConfig(req, "postgresql")

		decodedConfig := config.(PostgresqlConfig)

		assert.Equal(t, decodedConfig.PgDump.Enable, true)
	})

}

func TestTomlFileCreateFromReqConfigLog(t *testing.T) {
	fileName := "logtoml.toml"
	req := dc.AutomateConfig{
		Global: &shared.GlobalConfig{
			V1: &shared.V1{
				Log: &shared.Log{
					RedirectSysLog:      w.Bool(true),
					RedirectLogFilePath: w.String("/var/tmp/"),
				},
			},
		},
	}

	createTomlFileFromConfig(req, fileName)
	assert.FileExists(t, fileName)
	os.Remove(fileName)
}

func TestCheckIfRequestedIsCentrailisedLogging(t *testing.T) {
	file := []string{"../../pkg/testfiles/aws/centralised_log.toml"}
	val, err := checkIfRequestedConfigHasCentrailisedLogging(file)
	fmt.Println("err : ", err)
	assert.True(t, val)
}

func TestCheckIfRequestedIsCentrailisedLoggingWithPgConfig(t *testing.T) {
	file := []string{"../../pkg/testfiles/aws/pg.toml"}
	val, err := checkIfRequestedConfigHasCentrailisedLogging(file)
	fmt.Println("err : ", err)
	assert.Equal(t, val, false)
}

func TestCheckIfRequestedIsCentrailisedLoggingWithInvalidFile(t *testing.T) {
	file := []string{"../../pkg/testfiles/aws/centralised.toml"}
	_, err := checkIfRequestedConfigHasCentrailisedLogging(file)
	fmt.Println("err : ", err)
	assert.Error(t, err)
}

func TestCheckIfRequestedIsCentrailisedLoggingwitherror(t *testing.T) {
	file := []string{"../../pkg/testfiles/aws/centralised_log.toml"}
	val, err := checkIfRequestedConfigHasCentrailisedLogging(file)
	fmt.Println("err : ", err)
	assert.True(t, val)
}

func TestErrorOnSelfManaged(t *testing.T) {
	testCases := []struct {
		isPostgresql bool
		isOpenSearch bool
		errorWant    error
	}{
		{
			true,
			false,
			errors.Errorf(ERROR_SELF_MANAGED_CONFIG_SHOW, "Postgresql"),
		},
		{
			false,
			true,
			errors.Errorf(ERROR_SELF_MANAGED_CONFIG_SHOW, "OpenSearch"),
		},
		{
			false,
			false,
			nil,
		},
	}

	for _, testCase := range testCases {
		errGot := errorOnSelfManaged(testCase.isPostgresql, testCase.isOpenSearch)
		if errGot == nil {
			assert.Equal(t, testCase.errorWant, errGot)
		} else {
			assert.EqualError(t, testCase.errorWant, errGot.Error())
		}
	}
}

func TestCheckOutputForError(t *testing.T) {
	testCases := []struct {
		outputMsg string
		isError   bool
		err       error
	}{
		{
			`New Error msg`,
			true,
			errors.New("New Error msg"),
		},
		{
			`New msg`,
			false,
			errors.New("New msg"),
		},
	}

	for _, testCase := range testCases {
		err := checkOutputForError(testCase.outputMsg)
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
		}
	}
}

func TestSetConfigForPostgresqlAndOpensearch(t *testing.T) {

	testCases := []struct {
		remoteService string
		timestamp     string
		sshUtil       SSHUtil
		hostIP        string
		tomlFilePath  string
		isError       bool
		err           error
	}{
		{
			"postgresql",
			"20060102150405",
			getMockSSHUtil(&SSHConfig{}, nil, "", nil),
			"127.0.0.1",
			"some_file_path",
			false,
			nil,
		},
		{
			"opensearch",
			"20060102150405",
			getMockSSHUtil(&SSHConfig{}, errors.Errorf("remote copy"), "", nil),
			"127.0.0.2",
			"some_file_path",
			true,
			errors.Errorf("remote copy"),
		},
		{
			"opensearch",
			"20060102150405",
			getMockSSHUtil(&SSHConfig{}, nil, "", errors.Errorf("remote execution")),
			"127.0.0.3",
			"some_file_path",
			true,
			errors.Errorf("remote execution"),
		},
	}

	for _, testCase := range testCases {
		err := setConfigForPostgresqlAndOpensearch(testCase.remoteService, testCase.timestamp, testCase.sshUtil, testCase.hostIP, testCase.tomlFilePath, getMockWriterImpl())
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
		}
	}
}

func TestSetConfigForFrontEndNodes(t *testing.T) {
	testCases := []struct {
		args          []string
		sshUtil       SSHUtil
		frontendIps   []string
		remoteService string
		timestamp     string
		isError       bool
		err           error
	}{
		{
			[]string{"some_args"},
			getMockSSHUtil(&SSHConfig{}, nil, "config set operation completed", nil),
			[]string{"127.0.0.3", "127.0.0.4", "127.0.0.5"},
			"automate",
			"20060102150405",
			false,
			nil,
		},
	}

	for _, testCase := range testCases {
		err := setConfigForFrontEndNodes(testCase.args, testCase.sshUtil, testCase.frontendIps, testCase.remoteService, testCase.timestamp, getMockWriterImpl())
		if testCase.isError {
			assert.Error(t, err)
			assert.EqualError(t, testCase.err, err.Error())
		} else {
			assert.NoError(t, err)
		}
	}
}

func TestPatchConfigCommand(t *testing.T) {
	infra := getMockInfra()
	file := "../../pkg/testfiles/aws/centralised_log.toml"
	tests := []struct {
		testName     string
		infra        *AutomateHAInfraDetails
		sshUtil      SSHUtil
		args         []string
		isAutomate   bool
		isChefServer bool
		iPostgresql  bool
		isOpenSearch bool
		wantErr      bool
	}{
		{
			"patch for pg",
			infra,
			getMockSSHUtil(&SSHConfig{}, nil, "", nil),
			[]string{file},
			false,
			false,
			true,
			false,
			true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.testName, func(t *testing.T) {
			if err := runPatchCommand(&cobra.Command{}, tt.args); (err != nil) != tt.wantErr {
				fmt.Println(" err msg : ", err)
				t.Errorf("runInitConfigHACmd() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
	// configCmdFlags.postgresql = true
	// err := runPatchCommand(nil, nil)
	// assert.Equal(t, err.Error(), "Config file should be passed with ha-deployment-config flag")
}
func TestCheckUserConfigHasOnlyCentrailisedLogConfig(t *testing.T) {
	//var args []string
	t.Run("Returning false as the file is not empty", func(t *testing.T) {
		fileContent := `[global.v1.log]
		compress_rotated_logs = true
		max_number_rotated_logs = 10
		max_size_rotate_logs = "10M"
		redirect_log_file_path = "/var/tmp/"
		redirect_sys_log = true`
		tmpfile, err := ioutil.TempFile("", "test-output.toml")
		require.NoError(t, err)

		n, err := tmpfile.Write([]byte(fileContent))
		require.NoError(t, err)
		require.NotZero(t, n)

		err = tmpfile.Close()
		require.NoError(t, err)

		actualInfraDetails, err := checkUserConfigHasOnlyCentrailisedLogConfig(tmpfile.Name())
		fmt.Println("actualInfraDetails : ", actualInfraDetails)
		require.NoError(t, err)
		require.Equal(t, actualInfraDetails, false)
		require.NoError(t, err)
		err = os.Remove(tmpfile.Name())
		require.NoError(t, err)
	})
	t.Run("Returning true as the file is empty", func(t *testing.T) {
		fileContent := ``
		tmpfile, err := ioutil.TempFile("", "test-output.toml")
		require.NoError(t, err)

		_, err = tmpfile.Write([]byte(fileContent))
		require.NoError(t, err)

		err = tmpfile.Close()
		require.NoError(t, err)

		isOnlyCentralisedLog, err := checkUserConfigHasOnlyCentrailisedLogConfig(tmpfile.Name())
		fmt.Println("actualInfraDetails : ", isOnlyCentralisedLog)
		require.NoError(t, err)
		require.Equal(t, isOnlyCentralisedLog, true)
		require.NoError(t, err)
		err = os.Remove(tmpfile.Name())
		require.NoError(t, err)
	})
	t.Run("File not found", func(t *testing.T) {
		filePath := "testdata/notfound.json"
		_, err := checkUserConfigHasOnlyCentrailisedLogConfig(filePath)
		require.Error(t, err)
	})
}

func TestPatchCentralisedLoggingForBackend(t *testing.T) {
	infra := getMockInfra()
	file := "../../pkg/testfiles/aws/centralised_log.toml"
	postgresqlFlag := true
	openSearchFlag := true
	tests := []struct {
		testName     string
		infra        *AutomateHAInfraDetails
		sshUtil      SSHUtil
		args         []string
		isAutomate   bool
		isChefServer bool
		iPostgresql  bool
		isOpenSearch bool
		wantErr      bool
	}{
		{
			"patch centralised log for pg",
			infra,
			getMockSSHUtil(&SSHConfig{}, nil, "", nil),
			[]string{file},
			false,
			false,
			postgresqlFlag,
			false,
			true,
		},
		{
			"patch centralised log for os",
			infra,
			getMockSSHUtil(&SSHConfig{}, nil, "", nil),
			[]string{file},
			false,
			false,
			false,
			openSearchFlag,
			true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.testName, func(t *testing.T) {
			_ , err := patchAndRemoveCentralisedLoggingForBackend(tt.args,infra)
				fmt.Println(" err msg : ", err)
				assert.Nilf(t,err,"Unable to created toml file for postgresql toml")
			
		})
	}

}

func getMockSSHUtil(sshConfig *SSHConfig, CFTRError error, CSECOROutput string, CSECORError error) *MockSSHUtilsImpl {
	return &MockSSHUtilsImpl{
		getSSHConfigFunc: func() *SSHConfig {
			return sshConfig
		},
		copyFileToRemoteFunc: func(srcFilePath string, destFileName string, removeFile bool) error {
			return CFTRError
		},
		connectAndExecuteCommandOnRemoteFunc: func(remoteCommands string, spinner bool) (string, error) {
			return CSECOROutput, CSECORError
		},
	}
}
