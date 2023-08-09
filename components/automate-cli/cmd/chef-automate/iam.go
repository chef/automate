package main

import (
	"context"
	"fmt"
	"os"
	"regexp"
	"strings"

	"github.com/golang/protobuf/ptypes/wrappers"
	"github.com/spf13/cobra"

	iam_common "github.com/chef/automate/api/external/iam/v2/common"
	iam_req "github.com/chef/automate/api/external/iam/v2/request"
	v2_constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/automate-cli/pkg/adminmgmt"
	"github.com/chef/automate/components/automate-cli/pkg/client/apiclient"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-cli/pkg/status"
)

const adminsID = "admins"

var iamCmdFlags = struct {
	dryRun            bool
	adminToken        bool
	tokenID           string
	betaVersion       bool
	skipLegacyUpgrade bool
}{}

func newIAMCommand() *cobra.Command {
	return &cobra.Command{
		Use:               "iam COMMAND",
		Short:             "Chef Automate iam commands",
		PersistentPreRunE: preIAMCmd,
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
}

func newIAMAdminAccessCommand() *cobra.Command {
	return &cobra.Command{
		Use:   "admin-access COMMAND",
		Short: "Manage and restore default admin access",
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
}

func newIAMTokensCommand() *cobra.Command {
	return &cobra.Command{
		Use:   "token COMMAND",
		Short: "Manage tokens",
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
}

func newIAMCreateTokenCommand() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "create NAME",
		Short: "Generate a token",
		RunE:  runCreateTokenCmd,
		Args:  cobra.ExactArgs(1),
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
	cmd.PersistentFlags().BoolVar(
		&iamCmdFlags.adminToken,
		"admin",
		false,
		"Generate a token and add it to the chef-managed admin policy")
	cmd.PersistentFlags().StringVar(
		&iamCmdFlags.tokenID,
		"id",
		"",
		"Specify a custom ID (if omitted, an ID will be generated based on NAME)")
	return cmd
}

func newIAMRestoreDefaultAdminAccessCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "restore PASSWORD",
		Short: "Restore the factory default admin user, team, and access",
		Long: "Recreate the admin user, admin team, and related admin policy as needed " +
			"to restore to factory default and update the admin user's password",
		RunE: runRestoreDefaultAdminAccessAdminCmd,
		Args: cobra.ExactArgs(1),
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
	cmd.PersistentFlags().BoolVar(
		&iamCmdFlags.dryRun,
		"dry-run",
		false,
		"Show what would be updated by this command without performing any changes")
	return cmd
}

func newIAMVersionCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "version",
		Short: "Retrieve IAM version in use",
		RunE:  runIAMVersionCmd,
		Args:  cobra.ExactArgs(0),
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
}

func init() {
	iamCommand := newIAMCommand()
	iamCommand.AddCommand(newIAMVersionCmd())

	iamAdminAccessCommand := newIAMAdminAccessCommand()
	iamCommand.AddCommand(iamAdminAccessCommand)
	iamAdminAccessCommand.AddCommand(newIAMRestoreDefaultAdminAccessCmd())

	iamTokensCommand := newIAMTokensCommand()
	iamCommand.AddCommand(iamTokensCommand)
	iamTokensCommand.AddCommand(newIAMCreateTokenCommand())

	RootCmd.AddCommand(iamCommand)
}

type vsn struct {
	Major iam_common.Version_VersionNumber
	Minor iam_common.Version_VersionNumber
}

func display(v *iam_common.Version) string {
	x := vsn{Minor: v.Minor, Major: v.Major}
	switch x {
	case vsn{Major: iam_common.Version_V2, Minor: iam_common.Version_V1}:
		return "v2.1"
	// this should not happen unless forced at the database level
	case vsn{Major: iam_common.Version_V2, Minor: iam_common.Version_V0}:
		return "v2.0"
	default:
		return "v1.0"
	}
}

func runIAMVersionCmd(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	apiClient, err := apiclient.OpenConnection(ctx)
	if err != nil {
		return status.Wrap(err, status.APIUnreachableError,
			"Failed to create a connection to the API")
	}

	resp, err := apiClient.PoliciesClient().GetPolicyVersion(ctx, &iam_req.GetPolicyVersionReq{})
	if err != nil {
		return status.Wrap(err, status.APIError, "Failed to retrieve IAM version")
	}
	writer.Printf("IAM %s\n", display(resp.Version))
	return nil
}

func runRestoreDefaultAdminAccessAdminCmd(cmd *cobra.Command, args []string) error {
	if iamCmdFlags.dryRun {
		writer.Title("Dry run: showing all actions needed to restore default admin access without performing any changes\n")
	} else {
		writer.Title("Restoring all factory defaults for admin access\n")
	}
	newAdminPassword := args[0]
	ctx := context.Background()

	apiClient, err := apiclient.OpenConnection(ctx)
	if err != nil {
		return status.Wrap(err, status.APIUnreachableError, "Failed to create a connection to the API")
	}

	// restore admin user and team if needed
	membershipID, adminUserFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx,
		apiClient, newAdminPassword, iamCmdFlags.dryRun)
	if err != nil {
		return err
	}

	if adminUserFound {
		writer.Success("Updated existing admin user's password")
	} else {
		writer.Success("Created new admin user with specified password")
	}

	adminsTeamFound, err := adminmgmt.CreateAdminTeamIfMissing(ctx, apiClient, iamCmdFlags.dryRun)
	if err != nil {
		return err
	}

	if adminsTeamFound {
		writer.Skipped("Found admins team")
	} else {
		writer.Success("Recreated admins team")
	}

	// In dry-run mode, we might be missing some IDs that would have been created.
	// We'll only hit this condition in dry-run mode.
	if iamCmdFlags.dryRun && (membershipID == "" || !adminsTeamFound) {
		writer.Success("Added admin user to admins team")
	} else { // non-dry-run mode or dry-run mode where user and team already existed.
		userAdded, err := adminmgmt.AddAdminUserToTeam(ctx,
			apiClient, adminsID, membershipID, iamCmdFlags.dryRun)
		if err != nil {
			return err
		}

		if userAdded {
			writer.Success("Added admin user to admins team")
		} else {
			writer.Skipped("Admin user already existed in admins team")
		}
	}

	foundAdminsTeaminV2AdminPolicy, err := adminmgmt.UpdateAdminsPolicyIfNeeded(ctx,
		apiClient, iamCmdFlags.dryRun)
	if err != nil {
		return err
	}

	if !foundAdminsTeaminV2AdminPolicy {
		writer.Success("Added local 'admins' team to Chef-managed 'Administrator' policy")
	}

	writer.Skipped("Found local 'admins' team in Chef-managed 'Administrator' policy")

	if err := apiClient.CloseConnection(); err != nil {
		return status.Wrap(err, status.APIUnreachableError, "Failed to close connection to the API")
	}

	return nil
}

func runCreateTokenCmd(cmd *cobra.Command, args []string) error {
	name := args[0]

	ctx := context.Background()
	apiClient, err := apiclient.OpenConnection(ctx)
	if err != nil {
		return status.Wrap(err, status.APIUnreachableError,
			"Failed to create a connection to the API.")
	}

	var id string
	if iamCmdFlags.tokenID == "" {
		re := regexp.MustCompile(`[^a-z0-9]`)
		id = strings.ToLower(name)
		id = re.ReplaceAllString(id, "-")
	} else {
		id = iamCmdFlags.tokenID
	}

	tokenResp, err := apiClient.TokensClient().CreateToken(ctx, &iam_req.CreateTokenReq{
		Id:     id,
		Name:   name,
		Active: &wrappers.BoolValue{Value: true},
		// TODO (tc): Might want to let them specify a --projects list somehow eventually.
		Projects: []string{},
	})
	if err != nil {
		return status.Wrap(err, status.APIError, "Failed to generate new token.")
	}

	if iamCmdFlags.adminToken {
		member := fmt.Sprintf("token:%s", tokenResp.Token.Id)
		_, err = apiClient.PoliciesClient().AddPolicyMembers(ctx, &iam_req.AddPolicyMembersReq{
			Id:      v2_constants.AdminPolicyID,
			Members: []string{member},
		})
		if err != nil {
			return status.Wrap(err, status.APIError, "Failed to add token as a member of chef-managed Admin policy.")
		}
	}
	status.GlobalResult = struct {
		Token string `json:"token"`
	}{Token: tokenResp.Token.Value}
	writer.Println(tokenResp.Token.Value)
	return nil
}

func preIAMCmd(cmd *cobra.Command, args []string) error {
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}
	if isA2HARBFileExist() {
		output, err := RunCmdOnSingleAutomateNode(cmd, args)
		if err != nil {
			return err
		}
		writer.Print(output)
		// NOTE: used os.exit as need to stop next lifecycle method to execute
		os.Exit(0)
	}
	return nil
}
