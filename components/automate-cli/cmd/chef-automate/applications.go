package main

import (
	"context"
	"fmt"
	"io"
	"math"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/automate-cli/pkg/client/apiclient"
	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

const (
	SHOW_COMMAND = "chef-automate applications show-svcs"
)

type IExecutor interface {
	execCommand(string) (string, error)
	runCommandOnSingleAutomateNode(cmd *cobra.Command, args []string) (string, error)
}

type Executor struct{}

func init() {
	appsSubcmd := newApplicationsRootSubcmd()

	appsSubcmd.AddCommand(newApplicationsShowSvcsCmd())
	appsSubcmd.AddCommand(newApplicationsRemoveSvcsCmd())

	RootCmd.AddCommand(appsSubcmd)
}

func newApplicationsRootSubcmd() *cobra.Command {
	return &cobra.Command{
		Use:   "applications COMMAND",
		Short: "Manage applications observability features",
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}
}

type applicationsServiceFilters struct {
	disconnected   bool
	origin         string
	serviceName    string
	version        string
	channel        string
	application    string
	environment    string
	site           string
	buildTimestamp string
	groupName      string
}

// FilterApplied returns true if any member of applicationsServiceFilters is
// not the default value, i.e., the user has applied at least one filter
// criterion.
func (a applicationsServiceFilters) FilterApplied() bool {
	stringVals := []string{
		a.origin,
		a.serviceName,
		a.version,
		a.channel,
		a.application,
		a.environment,
		a.site,
		a.buildTimestamp,
		a.groupName,
	}
	for _, value := range stringVals {
		if value != "" {
			return true
		}
	}

	if a.disconnected {
		return true
	}

	return false
}

var applicationsServiceFiltersFlags = applicationsServiceFilters{}

func addFilteringFlagsToCmd(cmd *cobra.Command) {
	cmd.PersistentFlags().BoolVarP(
		&applicationsServiceFiltersFlags.disconnected,
		"disconnected",
		"D",
		false,
		"Select only services that are disconnected",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.origin,
		"origin",
		"o",
		"",
		"Select only services where the origin matches the given pattern",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.serviceName,
		"service-name",
		"n",
		"",
		"Select only services where the name matches the given pattern",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.version,
		"version",
		"v",
		"",
		"Select only services where the package version matches the given pattern",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.channel,
		"channel",
		"c",
		"",
		"Select only services where the subscribed channel matches the given pattern",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.application,
		"application",
		"a",
		"",
		"Select only services where the application name matches the given pattern",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.environment,
		"environment",
		"e",
		"",
		"Select only services where the application environment matches the given pattern",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.site,
		"site",
		"s",
		"",
		"Select only services where the site matches the given pattern",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.buildTimestamp,
		"buildstamp",
		"b",
		"",
		"Select only services where the buildstamp matches the given pattern",
	)
	cmd.PersistentFlags().StringVarP(
		&applicationsServiceFiltersFlags.groupName,
		"group",
		"g",
		"",
		"Select only services where the group name (suffix) matches the given pattern",
	)
}

func newApplicationsShowSvcsCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "show-svcs",
		Short: "Show services in the applications database",
		Long: `
Display a list of the habitat services stored in the applications database.
`,
		PersistentPreRunE: checkLicenseStatusForExpiry,
		RunE:              runApplicationsShowSvcsCmd,
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}

	addFilteringFlagsToCmd(c)
	return c
}

type removeSvcsOptions struct {
	all bool
	yes bool
}

var removeSvcsFlags = removeSvcsOptions{}

func newApplicationsRemoveSvcsCmd() *cobra.Command {
	c := &cobra.Command{
		Use:   "remove-svcs",
		Short: "Remove services from the applications database",
		Long: `
Remove services from the applications database.

You must fully decommission services by retiring physical hardware, terminating
the VM or container, or by using 'hab svc unload', before using the
'remove-svcs' command. Services that are incompletely decommissioned will send
a health-check at the appointed time and Automate will re-add them to the
services database.
`,
		PersistentPreRunE: checkLicenseStatusForExpiry,
		RunE:              runApplicationsRemoveSvcsCmd,
		Annotations: map[string]string{
			docs.Tag: docs.BastionHost,
		},
	}

	addFilteringFlagsToCmd(c)

	c.PersistentFlags().BoolVar(
		&removeSvcsFlags.all,
		"all",
		false,
		"Delete all services in the database. This flag must be given if no other filter is given.",
	)

	c.PersistentFlags().BoolVarP(
		&removeSvcsFlags.yes,
		"yes",
		"y",
		false,
		"Delete the services without a confirmation prompt",
	)

	return c
}

func makeServicesReqWithFilters() *applications.ServicesReq {
	req := &applications.ServicesReq{}

	flags := applicationsServiceFiltersFlags

	// disconnected   bool
	if flags.disconnected {
		req.Filter = append(req.Filter, "status:disconnected")
	}
	// origin         string
	if flags.origin != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("origin:%s", flags.origin))
	}
	// serviceName    string
	if flags.serviceName != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("service:%s", flags.serviceName))
	}
	// version        string
	if flags.version != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("version:%s", flags.version))
	}
	// channel        string
	if flags.channel != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("channel:%s", flags.channel))
	}
	// application    string
	if flags.application != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("application:%s", flags.application))
	}
	// environment    string
	if flags.environment != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("environment:%s", flags.environment))
	}
	// site           string
	if flags.site != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("site:%s", flags.site))
	}
	// buildTimestamp string
	if flags.buildTimestamp != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("buildstamp:%s", flags.buildTimestamp))
	}
	// groupName      string
	if flags.groupName != "" {
		req.Filter = append(req.Filter, fmt.Sprintf("group:%s", flags.groupName))
	}

	return req
}

func showApplicationsHA(cmd *cobra.Command, args []string, e IExecutor) error {
	output, err := e.runCommandOnSingleAutomateNode(cmd, args)
	if err != nil {
		return err
	}
	writer.Print(output)
	return nil
}

func showApplicationsStandalone() error {
	s := &serviceSet{
		applicationsServiceFilters: applicationsServiceFiltersFlags,
	}
	err := s.Connect()
	if err != nil {
		return err
	}
	err = s.Load()
	if err != nil {
		return err
	}
	err = s.PrintTSV()
	if err != nil {
		return err
	}
	return nil
}

func runApplicationsShowSvcsCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if err := showApplicationsHA(cmd, args, Executor{}); err != nil {
			return err
		}
	} else {
		if err := showApplicationsStandalone(); err != nil {
			return err
		}
	}
	return nil
}

func showAndPrompt(cmd *cobra.Command, e IExecutor, w *cli.Writer) ([]string, bool, error) {
	var splittedString []string
	flags := GetEnabledFlags(cmd, map[string]int{"all": 1})
	showCmd := SHOW_COMMAND + flags
	output, err := e.execCommand(showCmd)
	if err != nil {
		return splittedString, false, err
	}
	splittedString = strings.Split(output, "\n")
	// splittedString always contains 2 lines no matter services matches the criteria or not
	// first line is table header and last line is empty line.
	// >2 means it's containing the services which matches the criteria.
	if len(splittedString) > 2 {
		for i := 0; i < len(splittedString); i++ {
			fmt.Println(splittedString[i])
		}
		prompt := fmt.Sprintf("The above %d services will be deleted. Do you wish to continue?", len(splittedString)-2)
		proceed, err := w.Confirm(prompt)
		if err != nil {
			return splittedString, false, err
		}
		if !proceed {
			return splittedString, false, nil
		}
	}
	return splittedString, true, nil
}

func removeApplicationsHA(cmd *cobra.Command, args []string, e IExecutor, w *cli.Writer) error {
	var splittedString []string
	// If user is not passing -y flag, then first we need to show the list with given conditions and then
	// give confirmation prompt
	if !removeSvcsFlags.yes {
		var proceed bool
		var err error
		splittedString, proceed, err = showAndPrompt(cmd, e, w)
		if err != nil {
			return err
		}
		if !proceed {
			return nil
		}
	}
	args = append(args, "-y")
	output, err := e.runCommandOnSingleAutomateNode(cmd, args)
	if err != nil {
		return err
	}
	// len(splittedString) == 2 means list is empty it's just containing table header and empty line which means we haven't
	// got any services to remove
	if removeSvcsFlags.yes || len(splittedString) == 2 {
		writer.Println(output)
	} else {
		writer.Println(fmt.Sprintf("Removed %d services", len(splittedString)-2))
	}

	return nil
}

func removeApplicationsStandalone() error {
	s := &serviceSet{
		applicationsServiceFilters: applicationsServiceFiltersFlags,
	}
	err := s.Connect()
	if err != nil {
		return err
	}
	err = s.Load()
	if err != nil {
		return err
	}

	if len(s.services) < 1 {
		fmt.Fprintln(os.Stderr, "No services match the criteria, nothing to delete")
		return nil
	}

	err = s.PrintTSV()
	if err != nil {
		return err
	}

	if !removeSvcsFlags.yes {
		fmt.Println("")
		prompt := fmt.Sprintf("The above %d services will be deleted. Do you wish to continue?", len(s.services))
		proceed, err := writer.Confirm(prompt)
		if err != nil {
			return err
		}
		if !proceed {
			return nil
		}
	}

	err = s.RemoveSet()
	if err != nil {
		return err
	}
	return nil
}

func runApplicationsRemoveSvcsCmd(cmd *cobra.Command, args []string) error {
	if !applicationsServiceFiltersFlags.FilterApplied() && !removeSvcsFlags.all {
		return errors.New("You must filter the services to be deleted or pass the --all flag to delete all services")
	}
	if isA2HARBFileExist() {
		if err := removeApplicationsHA(cmd, args, Executor{}, writer); err != nil {
			return err
		}
	} else {
		if err := removeApplicationsStandalone(); err != nil {
			return err
		}
	}

	return nil
}

type serviceSet struct {
	applicationsServiceFilters
	appsClient applications.ApplicationsServiceClient
	services   []*applications.Service
	ctx        context.Context
}

func (e Executor) execCommand(cmd string) (string, error) {
	output, err := exec.Command("/bin/sh", "-c", cmd).Output()
	if err != nil {
		return "", err
	}
	return string(output), nil
}

func (e Executor) runCommandOnSingleAutomateNode(cmd *cobra.Command, args []string) (string, error) {
	return RunCmdOnSingleAutomateNode(cmd, args)
}

func (s *serviceSet) Connect() error {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	apiClient, err := apiclient.OpenConnection(ctx)
	if err != nil {
		return err
	}

	s.ctx = ctx
	s.appsClient = apiClient.ApplicationsClient()

	return nil
}

func (s *serviceSet) Load() error {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	req := makeServicesReqWithFilters()
	streamIn, err := s.appsClient.FindServices(ctx, req)
	if err != nil {
		return err
	}
	for {
		svc, err := streamIn.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		s.services = append(s.services, svc)
	}

	return nil
}

// PrintTSV prints a plain text table of the services
//   - Header: match the widths to the first row so it looks ok
//   - Header: print it to stderr so you can do easy shell redirection/pipes for text
//     processing
//   - Print table rows as TSV for easier processing, even though the columns
//     won't align all the time.
func (s *serviceSet) PrintTSV() error {
	maxIDLength, maxGroupLength, maxReleaseLength, maxFqdnLength, maxApplicationLength, maxEnvironmentLength, maxStatusLabelLength := 0, 0, 0, 0, 0, 0, 0
	for _, t := range s.services {
		maxIDLength = int(math.Max(float64(maxIDLength), float64(len(t.Id))))
		maxGroupLength = int(math.Max(float64(maxGroupLength), float64(len(t.Group))))
		maxReleaseLength = int(math.Max(float64(maxReleaseLength), float64(len(t.Release))))
		maxFqdnLength = int(math.Max(float64(maxFqdnLength), float64(len(t.Fqdn))))
		maxApplicationLength = int(math.Max(float64(maxApplicationLength), float64(len(t.Application))))
		maxEnvironmentLength = int(math.Max(float64(maxEnvironmentLength), float64(len(t.Environment))))
		maxStatusLabelLength = int(math.Max(float64(maxStatusLabelLength), float64(len(s.statusLabelFor(t)))))
	}

	// calculate a format string with fixed width, right padded strings for each
	// of the columns
	fmtString := fmt.Sprintf("%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\n",
		maxIDLength,
		maxGroupLength,
		maxReleaseLength,
		maxFqdnLength,
		maxApplicationLength+maxEnvironmentLength+1,
		maxStatusLabelLength,
	)

	fmt.Printf(fmtString, "id", "svc.group", "release", "FQDN", "app:env", "status")
	for _, svc := range s.services {
		fmt.Printf(fmtString,
			svc.Id,
			svc.Group,
			svc.Release,
			svc.Fqdn,
			svc.Application+":"+svc.Environment,
			s.statusLabelFor(svc),
		)
	}

	return nil
}

func (s *serviceSet) RemoveSet() error {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()

	toRemove := make([]string, len(s.services))
	for i, svc := range s.services {
		toRemove[i] = svc.Id
	}

	req := &applications.DeleteServicesByIDReq{
		Ids: toRemove,
	}
	res, err := s.appsClient.DeleteServicesByID(ctx, req)

	if err != nil {
		return err
	}

	fmt.Fprintf(os.Stderr, "Removed %d services\n", len(res.GetServices()))

	return nil
}

func (s *serviceSet) statusLabelFor(svc *applications.Service) string {
	if svc.Disconnected {
		return "DISCONNECTED"
	} else {
		return svc.HealthCheck.String()
	}
}
