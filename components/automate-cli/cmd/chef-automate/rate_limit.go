package main

import (
	"fmt"

	dc "github.com/chef/automate/api/config/deployment"
	config "github.com/chef/automate/api/config/shared"
)

const (
	restartJournaldService           = "sudo systemctl restart systemd-journald.service"
	journaldConfigFile               = "/etc/systemd/journald.conf.d/automate.conf"
	journaldConfigFilePath           = "/etc/systemd/journald.conf.d"
	defaultRateLimitBurstJournald    = int32(1000)
	defaultRateLimitIntervalJournald = int32(5000) // in ms
)

func removeRateLimitFile() string {
	return fmt.Sprintf("sudo rm -f %s ; \n %s; \n", journaldConfigFile, restartJournaldService)
}

func removeOrUpdateRateLimit(args []string, remoteType string, sshUtil SSHUtil, remoteIp []string) error {
	req, err := getConfigForArgsLogs(args, remoteType)
	if err != nil {
		return err
	}
	var scriptCommands string
	if req.GetGlobal().GetV1().GetLog().GetRateLimitBurst() == nil && req.GetGlobal().GetV1().GetLog().GetRateLimitInterval() == nil {
		scriptCommands = removeRateLimitFile()
	} else if req.GetGlobal().GetV1().GetLog().GetRateLimitBurst().GetValue() > 0 || req.GetGlobal().GetV1().GetLog().GetRateLimitInterval().GetValue() > 0 {
		scriptCommands = removeRateLimitFile()
		scriptCommands += createScriptCommandsForRateLimit(req)
	}
	if len(scriptCommands) == 0 {
		return nil
	}

	err = runScriptOnRemoteNode(sshUtil, remoteIp, scriptCommands, remoteType, false)
	if err != nil {
		return err
	}
	return nil
}

// left
func patchRateLimitForBackend(args []string, infra *AutomateHAInfraDetails) error {
	sshconfig := &SSHConfig{}
	sshconfig.sshUser = infra.Outputs.SSHUser.Value
	sshconfig.sshKeyFile = infra.Outputs.SSHKeyFile.Value
	sshconfig.sshPort = infra.Outputs.SSHPort.Value
	sshUtil := NewSSHUtil(sshconfig)

	var remoteService string
	var remoteIps []string
	if configCmdFlags.postgresql {
		remoteService = "postgresql"
		remoteIps = infra.Outputs.PostgresqlPrivateIps.Value
	}
	if configCmdFlags.opensearch {
		remoteService = "opensearch"
		remoteIps = infra.Outputs.OpensearchPrivateIps.Value
	}
	err := enableRateLimitConfigForHA(args, remoteService, sshUtil, remoteIps)
	if err != nil {
		return err
	}
	return nil
}

func removeRateLimiterConfig(args []string) (string, error) {
	var inputfile string
	var err error
	if configCmdFlags.postgresql {
		inputfile, err = removeCentralisedLogsandRateLimitFromUserConfigForPg(args[0])
		if err != nil {
			return "", err
		}
	}
	if configCmdFlags.opensearch {
		inputfile, err = removeCentralisedLogsandRateLimitFromUserConfigForOs(args[0])
		if err != nil {
			return "", err
		}
	}
	args[0] = inputfile
	return inputfile, nil
}

// enableRateLimitConfigForHA checks for requested and existing configuration for Rate Limiting
func enableRateLimitConfigForHA(args []string, remoteType string, sshUtil SSHUtil, remoteIp []string) error {
	reqConfig, err := getConfigForArgsLogs(args, remoteType)
	if err != nil {
		return err
	}
	//Returning if there is no config set for rateLimitBurst and rateLimitInterval
	if reqConfig.GetGlobal().GetV1().GetLog().GetRateLimitBurst() == nil && reqConfig.GetGlobal().GetV1().GetLog().GetRateLimitInterval() == nil {
		return nil
	}
	existConfig, err := getPostgresOrOpenSearchExistingLogConfig(remoteType)
	if err != nil {
		return err
	}

	err = enableRateLimit(reqConfig, existConfig, sshUtil, remoteIp, remoteType, args)
	if err != nil {
		return err
	}

	return nil
}

// getScriptCommandsForRateLimit gets the commands to be executed for rateLimit
func getScriptCommandsForRateLimitJournald(reqConfig *dc.AutomateConfig, existConfig *dc.AutomateConfig) string {
	var scriptCommands string
	if existConfig.GetGlobal().GetV1().GetLog() != nil {
		merged := &dc.AutomateConfig{}
		//Merging both the config into the requested config for comparing
		config.Merge(existConfig, reqConfig, merged)
		*reqConfig = *merged
		//If config changed reapplying the config accordingly
		if isConfigChanged(reqConfig.GetGlobal().GetV1().GetLog(), existConfig.GetGlobal().GetV1().GetLog()) {
			scriptCommands = createScriptCommandsForRateLimit(reqConfig)
		} else {
			//if config in unchanged do nothing and returns
			return ""
		}
	} else if existConfig.GetGlobal().GetV1().GetLog() == nil &&
		(reqConfig.GetGlobal().GetV1().GetLog().GetRateLimitBurst().GetValue() > 0 ||
			reqConfig.GetGlobal().GetV1().GetLog().GetRateLimitInterval().GetValue() > 0) {
		scriptCommands = createScriptCommandsForRateLimit(reqConfig)
	}

	return scriptCommands
}

// createConfigFileForJournald created a config file as /etc/systemd/journald.conf.d/automate.conf
func createConfigFileForJournald(rateLimitBurst int32, rateLimitInterval int32) string {
	return fmt.Sprintf(`[Journal]
RateLimitBurst=%d
RateLimitInterval=%dms
`, rateLimitBurst, rateLimitInterval)
}

// createScriptCommandsForRateLimit sets config for systemd-journald
func createScriptCommandsForRateLimit(reqConfig *dc.AutomateConfig) string {
	rateLimitBurstJournald, rateLimitIntervalJournald := getRateLimitValues(reqConfig, defaultRateLimitBurstJournald, defaultRateLimitIntervalJournald)
	contentForRateLimitConfig := createConfigFileForJournald(rateLimitBurstJournald, rateLimitIntervalJournald)

	//creating a file and adding content in the file
	JournaldCreateFileCommand := fmt.Sprintf("echo \"%s\" > %s", contentForRateLimitConfig, journaldConfigFile)

	return fmt.Sprintf("sudo mkdir -p %s ; \n sudo sh -c '%s'; \n %s;", journaldConfigFilePath, JournaldCreateFileCommand, restartJournaldService)
}

// enableRateLimit gets commands for systemd-journald rateLimit config
func enableRateLimit(reqConfig *dc.AutomateConfig, existConfig *dc.AutomateConfig, sshUtil SSHUtil, remoteIp []string, remoteType string, args []string) error {
	scriptCommands := getScriptCommandsForRateLimitJournald(reqConfig, existConfig)
	if scriptCommands == "" {
		//if there are no script commands do nothing
		return nil
	}

	// if centralized logging is off then only we will update the bastion log config (because for rateLimiter and centralized we are maintaing same file).
	// NOTE: we are passing args here instead of reqConfig. because we are updating reqConfig in above function(if exist config present).
	isCentralizedLoggerConfigPresent, err := checkIfRequestedConfigHasCentrailisedLogging(args)
	if err != nil {
		return err
	}

	err = runScriptOnRemoteNode(sshUtil, remoteIp, scriptCommands, remoteType, !isCentralizedLoggerConfigPresent)
	if err != nil {
		return err
	}

	// We also have to update the same values in centralized log because it's enabled
	if !isCentralizedLoggerConfigPresent && existConfig.GetGlobal().GetV1().GetLog().GetRedirectSysLog().GetValue() {
		scriptCommands := getScriptCommandsForLogging(reqConfig, existConfig)
		if scriptCommands == "" {
			//if there are no script commands do nothing
			return nil
		}

		// Passing false because we don't want to print it again, we are already printing it's patched for journald also
		// When user is only patching for centralised logging not rateLimit then only we will pass it as true
		err := runScriptOnRemoteNode(sshUtil, remoteIp, scriptCommands, remoteType, false)
		if err != nil {
			return err
		}
	}

	if !isCentralizedLoggerConfigPresent {
		updateTomlFileFromConfig(remoteType, reqConfig)
	}

	return nil
}

func updateTomlFileFromConfig(remoteType string, reqConfig *dc.AutomateConfig) {
	if remoteType == "postgresql" {
		_, err := createTomlFileFromConfig(&reqConfig, postgresLogConfig)
		if err != nil {
			writer.Errorf("Unable to created toml file for postgresql toml %v", err)
		}
	} else {
		_, err := createTomlFileFromConfig(&reqConfig, opensearchConfig)
		if err != nil {
			writer.Errorf("Unable to created toml file for postgresql toml %v", err)
		}
	}
}
