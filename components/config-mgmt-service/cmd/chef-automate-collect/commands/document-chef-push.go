package commands

import (
	"fmt"

	"github.com/spf13/cobra"

	apiReq "github.com/chef/automate/api/external/cfgmgmt/request"
)

var documentChefPushFlags = struct {
	verbose              bool
	lockfilePath         string // required
	policyNodeGroup      string // required
	policyDomainURL      string // required
	useSCMdescription    bool
	description          string // required if useSCMdescription is false
	policyDomainUsername string
}{}

func newDocumentChefPushCommand() *cobra.Command {
	c := &cobra.Command{
		Use:   "document-chef-push",
		Short: "Gather metadata and report an invocation of `chef push` to Chef Automate",
		RunE:  runDocumentChefPushCommand,
	}
	c.Flags().BoolVarP(&documentChefPushFlags.verbose,
		"verbose",
		"v",
		false,
		"show verbose output",
	)

	// Take the various attribute inputs as flags instead of args because we have
	// a lot of required attributes and flags is more descriptive and a little
	// less mistake-prone
	c.Flags().StringVarP(&documentChefPushFlags.lockfilePath,
		"lockfile",
		"l",
		"",
		"path to the lockfile (e.g., Policyfile.lock.json)",
	)
	c.Flags().StringVarP(&documentChefPushFlags.policyNodeGroup,
		"policy-node-group",
		"g",
		"",
		"policy node group (`policy group` in a Chef Server architecture)",
	)
	c.Flags().StringVarP(&documentChefPushFlags.policyDomainURL,
		"policy-domain-url",
		"s", // matches knife
		"",
		"policy domain URL (Chef Server URL including orgname path in a Chef Server architecture)",
	)
	c.Flags().StringVarP(&documentChefPushFlags.policyDomainUsername,
		"policy-domain-username",
		"u", // matches knife
		"",
		"policy domain username (Chef Server username in a Chef Server architecture)",
	)

	return c
}

func runDocumentChefPushCommand(cmd *cobra.Command, args []string) error {
	// TODO validate:
	// - path to the lockfile (could default to Policyfile.lock.json but nyaaaa)
	// - description (but we could if we are configured to use last git commit)
	// - policy node group
	// - policy domain URL
	// - policy domain username

	// Things that we should let the user override here:
	// - disable the whole thing
	// - set auth token

	meta, err := newRolloutMetadata(documentChefPushFlags.lockfilePath)
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
