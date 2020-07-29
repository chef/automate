package commands

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"net/http"
	"os"

	"github.com/spf13/cobra"

	apiReq "github.com/chef/automate/api/external/cfgmgmt/request"
	"github.com/chef/automate/lib/httputils"
)

var reportNewRolloutFlags = struct {
	verbose              bool
	lockfilePath         string // required
	policyNodeGroup      string // required
	policyDomainURL      string // required
	description          string // if not given, use last commit message instead
	policyDomainUsername string
}{}

type rStringVarP struct {
	name    string
	desc    string
	pointer *string
}

var requiredStringVarPList = []rStringVarP{}

func newReportNewRolloutCommand() *cobra.Command {
	c := &cobra.Command{
		Use:   "report-new-rollout",
		Short: "Gather metadata about a new Chef Infra code rollout and report it to Chef Automate",
		RunE:  runReportNewRolloutCommand,
	}
	c.Flags().BoolVarP(&reportNewRolloutFlags.verbose,
		"verbose",
		"v",
		false,
		"show verbose output",
	)

	requiredStringVarP := func(p *string, name, short, value, usage string) {
		c.Flags().StringVarP(p, name, short, value, usage)
		requiredStringVarPList = append(requiredStringVarPList, rStringVarP{name: name, pointer: p, desc: usage})
	}

	// Take the various attribute inputs as flags instead of args because we have
	// a lot of required attributes and flags is more descriptive and a little
	// less mistake-prone
	requiredStringVarP(&reportNewRolloutFlags.lockfilePath,
		"lockfile",
		"l",
		"",
		"path to the lockfile (e.g., Policyfile.lock.json)",
	)
	requiredStringVarP(&reportNewRolloutFlags.policyNodeGroup,
		"policy-node-group",
		"g",
		"",
		"policy node group (`policy_group` in a Chef Server architecture)",
	)
	requiredStringVarP(&reportNewRolloutFlags.policyDomainURL,
		"policy-domain-url",
		"s", // matches knife
		"",
		"policy domain URL (Chef Server URL including orgname path in a Chef Server architecture)",
	)
	requiredStringVarP(&reportNewRolloutFlags.policyDomainUsername,
		"policy-domain-username",
		"u", // matches knife
		"",
		"policy domain username (Chef Server username in a Chef Server architecture)",
	)

	c.Flags().StringVarP(&reportNewRolloutFlags.description,
		"description",
		"d",
		"",
		"description of the code changes being pushed",
	)

	return c
}

func runReportNewRolloutCommand(cmd *cobra.Command, args []string) error {
	cliIO.EnableVerbose = reportNewRolloutFlags.verbose

	if val, envGiven := os.LookupEnv(DisableReportNewRolloutEnvVar); envGiven && val != "false" {
		cliIO.verbose("Found environment setting %s=%q disabling metadata collector", DisableReportNewRolloutEnvVar, val)
		return nil
	}

	missingRequiredFlags := []rStringVarP{}
	for _, v := range requiredStringVarPList {
		if *v.pointer == "" {
			missingRequiredFlags = append(missingRequiredFlags, v)
		}
	}
	if len(missingRequiredFlags) > 0 {
		cliIO.msg("Some required flags were not set. You must provide values for the following flags:")
		for _, f := range missingRequiredFlags {
			cliIO.msg("  --%s\t%s", f.name, f.desc)
		}
		os.Exit(1)
	}

	loader := NewConfigLoader()
	err := loader.Load()
	reportNewRolloutFailErr(err, "failed to load chef-automate-collect configuration")

	meta, err := newRolloutMetadata(reportNewRolloutFlags.lockfilePath)
	reportNewRolloutFailErr(err, "failed to gather metadata from lockfile")

	err = meta.ReadPolicyfileMetadata()
	reportNewRolloutFailErr(err, "failed to gather metadata from lockfile")

	err = meta.ReadGitMetadata()
	reportNewRolloutFailErr(err, "failed to gather metadata from git")

	meta.ReadCIMetadata()

	reqData := apiReq.CreateRollout{
		PolicyName:           meta.PolicyName,
		PolicyNodeGroup:      reportNewRolloutFlags.policyNodeGroup,
		PolicyRevisionId:     meta.PolicyRevisionID,
		PolicyDomainUrl:      reportNewRolloutFlags.policyDomainURL,
		ScmType:              apiReq.SCMType(apiReq.SCMType_value[meta.SCMType]),
		ScmWebType:           apiReq.SCMWebType(apiReq.SCMWebType_value[meta.SCMWebType]),
		PolicyScmUrl:         meta.PolicySCMURL,
		PolicyScmWebUrl:      meta.PolicySCMWebURL,
		PolicyScmCommit:      meta.PolicySCMCommit,
		Description:          meta.PolicySCMCommitMessage,
		CiJobUrl:             meta.CiJobURL,
		CiJobId:              meta.CiJobID,
		ScmAuthorName:        meta.SCMAuthorName,
		ScmAuthorEmail:       meta.SCMAuthorEmail,
		PolicyDomainUsername: reportNewRolloutFlags.policyDomainUsername,
	}

	if reportNewRolloutFlags.description != "" {
		reqData.Description = reportNewRolloutFlags.description
	}

	automate := loader.LoadedConfig.Automate

	tr := httputils.NewDefaultTransport()
	tr.TLSClientConfig = &tls.Config{InsecureSkipVerify: automate.InsecureTLS}
	httpClient := &http.Client{Transport: tr}

	fmt.Println(automate.URL)
	url, err := automate.CreateRolloutURL()
	reportNewRolloutFailErr(err, fmt.Sprintf("invalid Automate URL %q", automate.URL))

	reqBytes, err := json.Marshal(reqData)
	reportNewRolloutFailErr(err, "failed to generate API request JSON")

	req, err := http.NewRequest("POST", url.String(), bytes.NewReader(reqBytes))
	if err != nil {
		reportNewRolloutFailErr(err, "failed to generate API request")
	}

	req.Header["Api-Token"] = []string{automate.authToken}

	response, err := httpClient.Do(req)
	if err != nil {
		reportNewRolloutFailErr(err, fmt.Sprintf("HTTP API request to %q failed", url))
	}
	defer func() {
		_ = response.Body.Close()
	}()

	switch response.StatusCode {
	case 200, 201: // success
	case 401:
		cliIO.msg("ERROR: API Request to %q failed with status code 401. Your auth_token may be invalid", url)
		os.Exit(1)
	case 403:
		cliIO.msg("ERROR: API Request to %q failed with status code 403. Your auth_token does not have permissions to create rollout records", url)
		os.Exit(1)
	case 500, 501, 502, 503, 504:
		cliIO.msg("ERROR: API Request to %q failed with status code %d. Your Chef Automate server is unavailable for requests at this time.", url, response.StatusCode)
		os.Exit(1)
	default:
		cliIO.msg("ERROR: API Request to %q returned status code %d", url, response.StatusCode)
	}

	return nil
}

func reportNewRolloutFailErr(err error, message string) {
	if err != nil {
		cliIO.msg("ERROR: report-new-rollout failed: %s - %s", message, err.Error())
		os.Exit(1)
	}
}
