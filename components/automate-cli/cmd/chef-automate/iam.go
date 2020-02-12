package main

import (
	"context"
	"fmt"
	"regexp"
	"strings"

	"github.com/golang/protobuf/ptypes/wrappers"
	"github.com/spf13/cobra"

	authz_constants "github.com/chef/automate/components/authz-service/constants"
	v2_constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/automate-cli/pkg/adminmgmt"
	client_type "github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-cli/pkg/client/apiclient"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	iam_common "github.com/chef/automate/components/automate-gateway/api/iam/v2/common"
	iam_req "github.com/chef/automate/components/automate-gateway/api/iam/v2/request"
)

var iamCmdFlags = struct {
	dryRun            bool
	adminToken        bool
	tokenID           string
	betaVersion       bool
	skipLegacyUpgrade bool
}{}

func newIAMCommand() *cobra.Command {
	return &cobra.Command{
		Use:   "iam COMMAND",
		Short: "Chef Automate iam commands",
	}
}

func newIAMAdminAccessCommand() *cobra.Command {
	return &cobra.Command{
		Use:   "admin-access COMMAND",
		Short: "Manage and restore default admin access",
	}
}

func newIAMTokensCommand() *cobra.Command {
	return &cobra.Command{
		Use:   "token COMMAND",
		Short: "Manage tokens",
	}
}

func newIAMCreateTokenCommand() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "create NAME",
		Short: "Generate a token",
		RunE:  runCreateTokenCmd,
		Args:  cobra.ExactArgs(1),
	}
	cmd.PersistentFlags().BoolVar(
		&iamCmdFlags.adminToken,
		"admin",
		false,
		"Generate a token and grant it admin-level permission (IAM v2)")
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
	}
	cmd.PersistentFlags().BoolVar(
		&iamCmdFlags.dryRun,
		"dry-run",
		false,
		"Show what would be updated by this command without performing any changes")
	return cmd
}

func newIAMUpgradeToV2Cmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "upgrade-to-v2",
		Short: "Upgrade to IAM v2",
		Long: "Upgrade to IAM v2 and migrate existing v1 policies. " +
			"On downgrade, any new v2 policies will be reverted.",
		RunE: runIAMUpgradeToV2Cmd,
		Args: cobra.ExactArgs(0),
	}
	cmd.PersistentFlags().BoolVar(
		&iamCmdFlags.skipLegacyUpgrade,
		"skip-policy-migration",
		false,
		"Do not migrate policies from IAM v1.")

	// this flag is deprecated and does nothing but we don't wanna error on it
	cmd.PersistentFlags().BoolVar(
		&iamCmdFlags.betaVersion,
		"beta2.1",
		false,
		"Upgrade to version 2.1 with beta project authorization.")
	_ = cmd.PersistentFlags().MarkHidden("beta2.1")

	return cmd
}

func newIAMVersionCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "version",
		Short: "Retrieve IAM version in use",
		RunE:  runIAMVersionCmd,
		Args:  cobra.ExactArgs(0),
	}
}

func init() {
	iamCommand := newIAMCommand()
	iamCommand.AddCommand(newIAMUpgradeToV2Cmd())
	iamCommand.AddCommand(newIAMVersionCmd())

	iamAdminAccessCommand := newIAMAdminAccessCommand()
	iamCommand.AddCommand(iamAdminAccessCommand)
	iamAdminAccessCommand.AddCommand(newIAMRestoreDefaultAdminAccessCmd())

	iamTokensCommand := newIAMTokensCommand()
	iamCommand.AddCommand(iamTokensCommand)
	iamTokensCommand.AddCommand(newIAMCreateTokenCommand())

	RootCmd.AddCommand(iamCommand)
}

// Note: the indentation is to keep this in line with writer.Body()
const alreadyMigratedMessage = "You are already on IAM version %s."

const failedToResetDomainMessage = "PLEASE RUN THIS COMMAND AGAIN: There was an unexpected error resetting the projects for %s"

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

func runIAMUpgradeToV2Cmd(cmd *cobra.Command, args []string) error {
	ctx := context.Background()
	apiClient, err := apiclient.OpenConnection(ctx)
	if err != nil {
		return err
	}

	writer.Println("Creating default teams Editors and Viewers...")
	for id, description := range map[string]string{
		"editors": "Editors",
		"viewers": "Viewers",
	} {
		_, found, err := adminmgmt.EnsureTeam(ctx, id, description, apiClient, false /* no dryrun here */)
		if err != nil {
			return err
		}
		if found {
			writer.Skippedf("%s team already exists", description)
		}
	}
	writer.Print("\n")

	writer.Print("Migrating existing teams...\n\n")
	_, err = apiClient.TeamsV2Client().ApplyV2DataMigrations(ctx,
		&iam_req.ApplyV2DataMigrationsReq{})
	if err != nil {
		return status.Wrap(err, status.IAMUpgradeV2DatabaseError,
			"Failed to migrate teams service")
	}

	writer.Success("Enabled IAM v2.1")
	return nil
}

func outputReport(report string) {
	// if it's got ":" in it, split on the first
	parts := strings.SplitN(report, ":", 2)
	writer.Body(parts[0])
	if len(parts) >= 2 {
		writer.Body(strings.TrimSpace(parts[1]))
	}
}

func resetDomainsToV1(ctx context.Context, apiClient client_type.APIClient) error {
	_, err := apiClient.TeamsV2Client().ResetAllTeamProjects(ctx, &iam_req.ResetAllTeamProjectsReq{})
	if err != nil {
		writer.Failf(failedToResetDomainMessage, "teams")
		return err
	}

	_, err = apiClient.TokensV2Client().ResetAllTokenProjects(ctx, &iam_req.ResetAllTokenProjectsReq{})
	if err != nil {
		writer.Failf(failedToResetDomainMessage, "tokens")
		return err
	}
	return nil
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
	userID, adminUserFound, err := adminmgmt.CreateAdminUserOrUpdatePassword(ctx,
		apiClient, newAdminPassword, iamCmdFlags.dryRun)
	if err != nil {
		return err
	}

	if adminUserFound {
		writer.Success("Updated existing admin user's password")
	} else {
		writer.Success("Created new admin user with specified password")
	}

	adminsTeamID, adminsTeamFound, err := adminmgmt.CreateAdminTeamIfMissing(ctx,
		apiClient, iamCmdFlags.dryRun)
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
	if iamCmdFlags.dryRun && (userID == "" || adminsTeamID == "") {
		writer.Success("Added admin user to admins team")
	} else { // non-dry-run mode or dry-run mode where user and team already existed.
		userAdded, err := adminmgmt.AddAdminUserToTeam(ctx,
			apiClient, adminsTeamID, userID, iamCmdFlags.dryRun)
		if err != nil {
			return err
		}

		if userAdded {
			writer.Success("Added admin user to admins team")
		} else {
			writer.Skipped("Admin user already existed in admins team")
		}
	}

	// grant access to admins team if needed
	resp, err := apiClient.PoliciesClient().GetPolicyVersion(ctx, &iam_req.GetPolicyVersionReq{})
	if err != nil {
		return status.Wrap(err, status.APIError, "Failed to verify IAM version")
	}

	writer.Titlef("Checking IAM %s policies for admin policy with admins team.\n", display(resp.Version))

	switch resp.Version.Major {
	case iam_common.Version_V1:
		foundV1AdminPolicy, createdNewV1Policy, err := adminmgmt.UpdateV1AdminsPolicyIfNeeded(ctx,
			apiClient, iamCmdFlags.dryRun)
		if err != nil {
			return err
		}

		if foundV1AdminPolicy {
			writer.Skipped("Found admin policy that contains the admins team")
		} else {
			// Note: (tc) This should never happen currently since we currently don't support
			// editing policies but adding for future-proofing against the functionality.
			// Note: (sr) PurgeSubjectFromPolicies can alter policies -- when a user or a
			// team is removed; so, this could be more realistic than we think.
			writer.Successf("Found default admins team policy but it did not contain "+
				"the admins team subject (%s). Added admins team to default admin policy.",
				authz_constants.LocalAdminsTeamSubject)
		}
		if createdNewV1Policy {
			writer.Success("Created new admins policy")
		}
	case iam_common.Version_V2:
		foundAdminsTeaminV2AdminPolicy, err := adminmgmt.UpdateV2AdminsPolicyIfNeeded(ctx,
			apiClient, iamCmdFlags.dryRun)
		if err != nil {
			return err
		}

		if !foundAdminsTeaminV2AdminPolicy {
			writer.Success("Added local team: admins to Chef-managed policy: Admin")
		}

		writer.Skipped("Found local team: admins in Chef-managed policy: Admin")
	default:
		// do nothing
	}

	if err := apiClient.CloseConnection(); err != nil {
		return status.Wrap(err, status.APIUnreachableError, "Failed to close connection to the API")
	}

	return nil
}

const adminTokenIAMPreconditionError = "`chef-automate iam token create NAME --admin` is an IAM v2 command.\n" +
	"For v1 use `chef-automate admin-token`.\n"

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

	tokenResp, err := apiClient.TokensV2Client().CreateToken(ctx, &iam_req.CreateTokenReq{
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
		resp, err := apiClient.PoliciesClient().GetPolicyVersion(ctx, &iam_req.GetPolicyVersionReq{})
		if err != nil {
			return status.Wrap(err, status.APIError, "Failed to retrieve IAM version")
		}
		if resp.Version.Major == iam_common.Version_V1 {
			return status.New(status.APIError, adminTokenIAMPreconditionError)
		}

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
