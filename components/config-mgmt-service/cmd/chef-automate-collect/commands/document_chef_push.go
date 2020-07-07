package commands

import (
	"fmt"

	"github.com/spf13/cobra"

	apiReq "github.com/chef/automate/api/external/cfgmgmt/request"
)

func newDocumentChefPushCommand() *cobra.Command {
	c := &cobra.Command{
		Use:   "document-chef-push",
		Short: "Gather metadata and report an invocation of `chef push` to Chef Automate",
		RunE:  runDocumentChefPushCommand,
	}
	return c
}

func runDocumentChefPushCommand(cmd *cobra.Command, args []string) error {
	// Inputs we need that have no defaults
	// - path to the lockfile (could default to Policyfile.lock.json but nyaaaa)
	// - description (but we could if we are configured to use last git commit)
	// - policy node group
	// - policy domain URL
	// - policy domain username

	// Things that we should let the user override here:
	// - config file to use
	// - disable the whole thing
	// - set auth token

	// FIXME: validate it
	policyLockPathIn := args[0]

	meta, err := newRolloutMetadata(policyLockPathIn)
	describeCmdFailErr(err)

	err = meta.ReadPolicyfileMetadata()
	describeCmdFailErr(err)

	err = meta.ReadGitMetadata()
	describeCmdFailErr(err)

	meta.ReadCIMetadata()

	reqData := apiReq.CreateRollout{
		PolicyName:           meta.PolicyName,
		PolicyNodeGroup:      meta.PolicyNodeGroup, // FIXME
		PolicyRevisionId:     meta.PolicyRevisionID,
		PolicyDomainUrl:      meta.PolicyDomainURL, // FIXME
		ScmType:              apiReq.SCMType(apiReq.SCMType_value[meta.SCMType]),
		ScmWebType:           apiReq.SCMWebType(apiReq.SCMWebType_value[meta.SCMWebType]),
		PolicyScmUrl:         meta.PolicySCMURL,
		PolicyScmWebUrl:      meta.PolicySCMWebURL,
		PolicyScmCommit:      meta.PolicySCMCommit,
		Description:          "FIXME", // FIXME
		CiJobUrl:             meta.CiJobURL,
		CiJobId:              meta.CiJobID,
		ScmAuthorName:        meta.SCMAuthorName,
		ScmAuthorEmail:       meta.SCMAuthorEmail,
		PolicyDomainUsername: "", // FIXME
	}

	fmt.Printf("%+v\n\n", reqData)

	return nil
}
