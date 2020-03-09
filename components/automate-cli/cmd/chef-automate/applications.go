package main

import (
	"context"
	"fmt"
	"io"
	"os"
	"time"

	"github.com/pkg/errors"
	"github.com/spf13/cobra"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/automate-cli/pkg/client/apiclient"
)

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
		RunE: runApplicationsShowSvcsCmd,
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
		RunE: runApplicationsRemoveSvcsCmd,
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

func runApplicationsShowSvcsCmd(cmd *cobra.Command, args []string) error {
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

func runApplicationsRemoveSvcsCmd(cmd *cobra.Command, args []string) error {
	if !applicationsServiceFiltersFlags.FilterApplied() && !removeSvcsFlags.all {
		return errors.New("You must filter the services to be deleted or pass the --all flag to delete all services")
	}

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

type serviceSet struct {
	applicationsServiceFilters
	appsClient applications.ApplicationsServiceClient
	services   []*applications.Service
	ctx        context.Context
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
// * Header: match the widths to the first row so it looks ok
// * Header: print it to stderr so you can do easy shell redirection/pipes for text
//   processing
// * Print table rows as TSV for easier processing, even though the columns
//   won't align all the time.
func (s *serviceSet) PrintTSV() error {
	svc := &applications.Service{}
	if len(s.services) >= 1 {
		svc = s.services[0]
	}

	// calculate a format string with fixed width, right padded strings for each
	// of the columns
	fmtString := fmt.Sprintf("%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\n",
		len(svc.Id),
		len(svc.Group),
		len(svc.Release),
		len(svc.Fqdn),
		len(svc.Application)+len(svc.Environment)+1,
		len(s.statusLabelFor(svc)),
	)

	fmt.Fprintf(os.Stderr, fmtString, "id", "svc.group", "release", "FQDN", "app:env", "status")
	for _, svc := range s.services {
		fmt.Printf("%s\t%s\t%s\t%s\t%s:%s\t%s\n",
			svc.Id,
			svc.Group,
			svc.Release,
			svc.Fqdn,
			svc.Application, svc.Environment,
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
