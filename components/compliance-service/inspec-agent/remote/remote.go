package remote

import (
	"context"
	"encoding/json"
	"fmt"
	"net/url"
	"strings"

	"github.com/aws/aws-sdk-go/aws/awserr"
	uuid "github.com/gofrs/uuid"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/nodemanager-service/managers"
)

type RemoteJob struct {
	TokensMgmtClient authn.TokensMgmtClient
	AuthzClient      authz.AuthorizationClient
	AutomateFQDN     string
}

var RemoteJobInfo RemoteJob

func RunSSMJob(ctx context.Context, ssmJob *types.InspecJob) *inspec.Error {
	logrus.Debugf("Running ssm job: %+v", ssmJob)
	// 1. CREATE TOKEN
	// this is needed to hand over to inspec so it can report to automate
	if RemoteJobInfo.TokensMgmtClient == nil {
		logrus.Error("unable to create auth token")
		return translateToInspecErr(fmt.Errorf("unable to connect to auth client: aborting job run for job %+v", ssmJob))
	}
	token, err := RemoteJobInfo.TokensMgmtClient.CreateToken(ctx, &authn.CreateTokenReq{
		Description: "token for inspec to report to automate",
		Active:      true,
	})
	if err != nil {
		logrus.Errorf("unable to create auth token for reporting to automate: %s", err.Error())
		return translateToInspecErr(err)
	}
	ssmJob.Reporter.Token = token.GetValue()
	ssmJob.Reporter.Url = RemoteJobInfo.AutomateFQDN
	ssmJob.Reporter.ReportUUID = uuid.Must(uuid.NewV4()).String()
	script, scriptType, err := assembleRemoteJobConfigAndScript(ssmJob)
	if err != nil {
		logrus.Errorf("unable to assemble config and script for remote job: %s", err.Error())
		return translateToInspecErr(err)
	}

	// 2. CALL SSM API AND HAND JOB OVER
	jobErr := managers.SendRemoteExecutionJob(ctx, ssmJob, script, scriptType)

	// 3. DELETE THE TOKEN
	// the jobs are not returned until they have a status different from "Pending" or "InProgress",
	// because we poll for their status inside of sendJob, so now we can delete the token
	// (https://docs.aws.amazon.com/systems-manager/latest/APIReference/API_Command.html)
	_, err = RemoteJobInfo.TokensMgmtClient.DeleteToken(ctx, &authn.DeleteTokenReq{Id: token.GetId()})
	if err != nil {
		logrus.Errorf("unable to delete token for reporting to automate: %s", err.Error())
		return translateToInspecErr(err)
	}
	return translateToInspecErr(jobErr)
}

func translateToInspecErr(err error) *inspec.Error {
	if err == nil {
		return nil
	}
	if awsErr, ok := err.(awserr.Error); ok {
		if awsErr.Code() == "ThrottlingException" {
			// by returning a conn timeout error here, we ensure we will retry this job if retries
			// are available, which is what we want to do!
			return inspec.NewInspecError(inspec.CONN_TIMEOUT, awsErr.Message())
		}
	}
	return inspec.NewInspecError(inspec.UNKNOWN_ERROR, err.Error())
}

func assembleRemoteJobConfigAndScript(job *types.InspecJob) (string, string, error) {
	// script to run
	profilesString := strings.Join(job.Profiles, " ")
	automateUrl, err := url.Parse(fmt.Sprintf("%s/data-collector/v0", job.Reporter.Url))
	if err != nil {
		return "", "", errors.Wrap(err, "assembleRemoteJobConfigAndScript unable to parse url")
	}

	var jsonConf []byte

	config := assembleConfig(job, automateUrl)
	jsonConf, err = json.Marshal(config)
	if err != nil {
		return "", "", errors.Wrap(err, "assembleRemoteJobConfigAndScript unable to marshal config information")
	}

	logrus.Infof("assembling script info with instance_id: %s for node_uuid: %s with node_name: %s and profiles: %s for reporting to: %s with report_uuid %s, using %s backend", job.SourceID, job.NodeID, job.NodeName, job.Profiles, automateUrl.String(), job.Reporter.ReportUUID, job.TargetConfig.Backend)

	switch job.TargetConfig.Backend {
	case inspec.BackendSSM, inspec.BackendAZ:
		return fmt.Sprintf(`#!/bin/bash

			# make sure inspec is installed
			curl -L https://omnitruck.chef.io/install.sh | sudo bash -s -- -s once -v '%s' -P inspec

			# inspec will return non-zero exit codes for control failures (e.g. 100, 101)
			# we don't want to fail the scan job for failed controls, so we overwrite those
			# exit codes with a 0
			modify_exit_code() {
				status=$1
				if [ $status -eq 1 ]; then
						exit 1
				fi
				exit 0
			}
			echo '%s' | sudo CHEF_LICENSE="accept-no-persist" inspec exec %s --json-config=-

			modify_exit_code $?`, job.RemoteInspecVersion, string(jsonConf), profilesString), inspec.BashScript, nil
	case inspec.BackendSSMWindows, inspec.BackendAZWindows:
		return fmt.Sprintf(`
				$global:InspecBinaryLocation = "$env:systemdrive\opscode\inspec\bin\inspec"
				Function Ensure-InspecInstalled {
						. { Invoke-WebRequest -UseBasicParsing https://omnitruck.chef.io/install.ps1 } | iex; install -project inspec -channel stable -install_strategy once -version '%s';
				}
				Function Invoke-InspecCommand {
						param(
								[Parameter(Mandatory=$false)]
								$Command = $null,
								[Parameter(Mandatory=$false)]
								$Path = $null,
								[Parameter(Mandatory=$false)]
								$JsonConfig
						)
						if($JsonConfig -ne $null) { $JsonArgument = '--json-config="-"' }
						Invoke-Expression -Command "echo '$JsonConfig' | $InspecBinaryLocation.bat $Command $Path $JsonArgument".Trim()
				}
				$env:CHEF_LICENSE="accept-no-persist"
				Ensure-InspecInstalled
				Invoke-InspecCommand -Command exec -Path "%s" -JsonConfig '%s'`, job.RemoteInspecVersion, profilesString, string(jsonConf)), inspec.PowershellScript, nil
	}
	// we really shouldn't ever get here, since this function is only called for ssm jobs
	return "", "", fmt.Errorf("invalid job target config backend: %s", job.TargetConfig.Backend)
}

func assembleConfig(job *types.InspecJob, automateUrl *url.URL) types.SSMTargetConfig {
	config := types.SSMTargetConfig{}
	config.BackendCache = true
	config.Sudo = job.TargetConfig.Sudo
	config.SudoPassword = job.TargetConfig.SudoPassword
	config.Reporter = map[string]inspec.Reporter{}
	config.Reporter["automate"] = inspec.Reporter{
		Url:         automateUrl.String(),
		Token:       job.Reporter.Token,
		NodeName:    job.NodeName,
		NodeID:      job.NodeID,
		Environment: job.NodeEnv,
		ReportUUID:  job.Reporter.ReportUUID,
		JobUUID:     job.JobID,
	}
	if len(job.ProfilesOwner) > 0 {
		config.Compliance = &types.AutomateLoginInfo{
			Server:   job.Reporter.Url,
			Token:    job.Reporter.Token,
			User:     job.ProfilesOwner,
			Insecure: true,
		}
	}
	return config
}
