package main

import (
	"bytes"
	"fmt"
	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	config "github.com/chef/automate/api/config/shared"
	"github.com/chef/toml"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"io/ioutil"
)

const (
	rsyslogConfigFile    = "/etc/rsyslog.d/automate.conf"
	logRotateConfigFile  = "/etc/logrotate.d/automate"
	postgresLogConfig    = "/hab/a2_deploy_workspace/postgres_log.toml"
	opensearchConfig     = "/hab/a2_deploy_workspace/opensearch_log.toml"
	restartSyslogService = "sudo systemctl restart rsyslog.service"
)

//enableCentralizedLogConfigForHA checks for requested and existing configuration for logging
func enableCentralizedLogConfigForHA(args []string, remoteType string, sshUser string, sshPort string, sskKeyFile string, remoteIp []string) error {
	reqConfig, err := getConfigForArgsLogs(args, remoteType)
	if err != nil {
		return err
	}
	//Returning if there is no config set for logging
	if reqConfig.GetGlobal().GetV1().GetLog() == nil {

		return nil
	}
	existConfig, err := getPostgresOrOpenSearchExistingLogConfig(remoteType)
	if err != nil {
		return err
	}

	err = enableCentralizedLogging(reqConfig, existConfig, sshUser, sshPort, sskKeyFile, remoteIp, remoteType)
	if err != nil {
		return err
	}

	return nil
}

//enableCentralizedLogging gets commands for rsyslog ang logrotate
func enableCentralizedLogging(reqConfig *dc.AutomateConfig, existConfig *dc.AutomateConfig, sshUser string, sshPort string, sskKeyFile string, remoteIp []string, remoteType string) error {

	scriptCommands := getScriptCommandsForLogging(reqConfig, existConfig)
	if scriptCommands == "" {
		//if there are no script commands do nothing
		return nil
	}

	err := createRsyslogAndLogRotateConfig(sshUser, sshPort, sskKeyFile, remoteIp, scriptCommands)
	if err != nil {
		return err
	}

	if remoteType == "postgresql" {
		_, err := createTomlFileFromConfig(&reqConfig, postgresLogConfig)
		if err != nil {
			writer.Errorf("Unable to created toml file for postgresql toml %v", err)
		}
	} else {
		createTomlFileFromConfig(&reqConfig, opensearchConfig)
		writer.Errorf("Unable to created toml file for open-search toml %v", err)
	}
	return nil

}

//getScriptCommandsForLogging gets the commands to be executed for  logging
func getScriptCommandsForLogging(reqConfig *dc.AutomateConfig, existConfig *dc.AutomateConfig) string {
	var scriptCommands string

	if existConfig.GetGlobal().GetV1().GetLog() != nil {
		merged := &dc.AutomateConfig{}
		//Merging both the config into the requested config for comparing
		config.Merge(existConfig, reqConfig, merged)
		*reqConfig = *merged
		//If config changed reapplying the config accordingly
		if isConfigChanged(reqConfig, existConfig) {
			scriptCommands = getScriptCommandsForConfigChangedLogging(reqConfig, existConfig)
		} else {
			//if config in unchanged do nothing and returns
			return ""
		}
	} else if existConfig.GetGlobal().GetV1().GetLog() == nil && reqConfig.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true {
		reqConfig.GetGlobal().ValidateReDirectSysLogConfig()
		scriptCommands = createScriptCommandsForCentralizedLog(reqConfig)
	}

	return scriptCommands

}

//getScriptCommandsForConfigChangedLogging gets the script commands where only some values are changed
func getScriptCommandsForConfigChangedLogging(reqConfig *dc.AutomateConfig, existConfig *dc.AutomateConfig) string {
	var scriptCommands string
	//Disable the centralized logging if requested
	// if only logrorate policies are requested and there is no change in the file path created
	// if file path changes are requested
	if reqConfig.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == false &&
		existConfig.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() == true {
		scriptCommands = rollBackCentralized()
	} else if reqConfig.GetGlobal().GetV1().GetLog().GetRedirectLogFilePath().GetValue() == existConfig.GetGlobal().GetV1().GetLog().GetRedirectLogFilePath().GetValue() {
		logrotateFileCommand := fmt.Sprintf("echo \"%s\" > %s", configLogrotate(reqConfig.GetGlobal().GetV1().GetLog()), logRotateConfigFile)
		return fmt.Sprintf("sudo sh -c '%s'", logrotateFileCommand)

	} else {
		scriptCommands = createScriptCommandsForCentralizedLog(reqConfig)
	}
	return scriptCommands
}

// configLogrotate Adds a config file for logrotate as /etc/logrotate.d/automate
// it handles all the config for rotating logs.
func configLogrotate(req *shared.Log) string {
	var logRotateConfigContent string

	if req.GetCompressRotatedLogs().GetValue() == true {
		logRotateConfigContent = LogRotateConf(getLogFileName(req.GetRedirectLogFilePath().GetValue()),
			getConcatStringFromConfig("size", req.GetMaxSizeRotateLogs().GetValue()), getConcatStringFromConfig("rotate", req.GetMaxNumberRotatedLogs().GetValue()), "missingok", "copytruncate", "compress", "dateext")
	} else {
		logRotateConfigContent = LogRotateConf(getLogFileName(req.GetRedirectLogFilePath().GetValue()),
			getConcatStringFromConfig("size", req.GetMaxSizeRotateLogs().GetValue()), getConcatStringFromConfig("rotate", req.GetMaxNumberRotatedLogs().GetValue()), "missingok", "copytruncate", "dateext")
	}
	// Write the byteSlice to file
	logrus.Infof("log rotated file content created")

	return logRotateConfigContent
}

//createConfigFileForAutomateSysLog created a config file as /etc/rsyslog.d/automate.conf
// which redirects the logs to the specified location
func createConfigFileForAutomateSysLog(pathForLog string) string {
	return fmt.Sprintf(`if \$programname == \"bash\" then %s
& stop`, getLogFileName(pathForLog))

}

//LogRotateConf gets the log rotate configuration using the values  from config
func LogRotateConf(path string, params ...string) string {
	if len(params) < 1 {
		return ""
	}
	var buffer bytes.Buffer
	buffer.WriteString(path + " {" + "\n")
	for _, param := range params {
		buffer.WriteString("\t" + param + "\n")
	}
	buffer.WriteString("}" + "\n")
	return buffer.String()
}

//getLogFileName gets the log file name based on the path provided
func getLogFileName(path string) string {
	if string(path[len(path)-1]) == "/" {
		return fmt.Sprintf("%sautomate.log", path)
	} else {
		return fmt.Sprintf("%s/automate.log", path)
	}
}

func getConcatStringFromConfig(constant string, variable interface{}) string {
	return fmt.Sprintf("%s %v", constant, variable)
}

//decodeLogConfig decodes the log config from the log string from file
func decodeLogConfig(logConfig string) (*dc.AutomateConfig, error) {
	var src dc.AutomateConfig
	if _, err := toml.Decode(logConfig, &src); err != nil {
		return nil, err
	}

	return &src, nil
}

//getConfigForArgsLogs get the requested config from the patched file
func getConfigForArgsLogs(args []string, remoteService string) (*dc.AutomateConfig, error) {

	pemBytes, err := ioutil.ReadFile(args[0]) // nosemgrep
	if err != nil {
		return nil, err
	}

	destString := string(pemBytes)
	dest1, err := decodeLogConfig(destString)
	if err != nil {
		return nil, errors.Errorf("Config file must be a valid %s config", remoteService)
	}

	return dest1, nil

}

////rollBackCentralized removed the logrotate file and rsyslog file
func rollBackCentralized() string {
	rsyslogFileRemove := fmt.Sprintf("sudo rm %s", rsyslogConfigFile)

	logrorateFileRemove := fmt.Sprintf("sudo rm %s", logRotateConfigFile)

	return fmt.Sprintf(" %s; %s; %s", rsyslogFileRemove, logrorateFileRemove, restartSyslogService)
}

//setConfigForCentralizedLog sets config for rsyslog and logrorate
func createScriptCommandsForCentralizedLog(reqConfig *dc.AutomateConfig) string {
	contentForRsyslogConfig := createConfigFileForAutomateSysLog(reqConfig.GetGlobal().GetV1().GetLog().GetRedirectLogFilePath().GetValue())

	//creating a file and adding content in the file
	rsysCreateFileCommand := fmt.Sprintf("echo \"%s\" > %s", contentForRsyslogConfig, rsyslogConfigFile)

	//rsyslog command for creating and added the content
	logrotateFileCommand := fmt.Sprintf("echo \"%s\" > %s", configLogrotate(reqConfig.GetGlobal().GetV1().GetLog()), logRotateConfigFile)

	return fmt.Sprintf("sudo sh -c '%s'; \n sudo sh -c '%s'; \n %s;", rsysCreateFileCommand, logrotateFileCommand, restartSyslogService)

}

//createRsyslogAndLogRotateConfig patching the config into the remote database servers
func createRsyslogAndLogRotateConfig(sshUser string, sshPort string, sskKeyFile string, remoteIp []string, scriptCommands string) error {
	for i := 0; i < len(remoteIp); i++ {
		fmt.Println("Script commands got for this config", scriptCommands)
		output, err := ConnectAndExecuteCommandOnRemote(sshUser, sshPort, sskKeyFile, remoteIp[i], scriptCommands)
		if err != nil {
			writer.Errorf("%v", err)
			return err
		}
		writer.Printf(output)

	}
	return nil

}
