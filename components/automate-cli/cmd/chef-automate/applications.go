package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/spf13/cobra"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/components/automate-cli/pkg/client/apiclient"
)

func init() {
	appsSubcmd := newApplicationsRootSubcmd()

	appsSubcmd.AddCommand(newApplicationsShowSvcsCmd())

	RootCmd.AddCommand(appsSubcmd)
}

func newApplicationsRootSubcmd() *cobra.Command {
	return &cobra.Command{
		Use:    "applications COMMAND",
		Short:  "Manage applications visibility features",
		Hidden: true,
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
		Short: "Show services data collected by applications feature",
		RunE:  runApplicationsShowSvcsCmd,
	}

	addFilteringFlagsToCmd(c)
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
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	apiClient, err := apiclient.OpenConnection(ctx)
	if err != nil {
		return err
	}

	appsClient := apiClient.ApplicationsClient()

	// GetServices API is paginated; we find out how many services there are and
	// then set the page size to greater than that so we don't have to loop.
	// This should be revisited if this command is frequently used to fetch more
	// than 100k-ish services on RAM constrained machines
	statsReq := &applications.ServicesStatsReq{}
	stats, err := appsClient.GetServicesStats(ctx, statsReq)
	if err != nil {
		return err
	}

	servicesCount := stats.TotalServices

	req := makeServicesReqWithFilters()
	req.Pagination = &query.Pagination{Size: servicesCount + 100}

	res, err := apiClient.ApplicationsClient().GetServices(ctx, req)
	if err != nil {
		return err
	}

	// Plain text output as a table:
	// * Header: match the widths to the first row so it looks ok
	// * Header: print it to stderr so you can do easy shell redirection/pipes for text
	//   processing
	// * Print table rows as TSV for easier processing, even though the columns
	//   won't align all the time.
	svc := &applications.Service{}
	if len(res.Services) >= 1 {
		svc = res.Services[0]
	}

	// calculate a format string with fixed width, right padded strings for each
	// of the columns
	fmtString := fmt.Sprintf("%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\t%%-%ds\n",
		len(svc.Id),
		len(svc.Group),
		len(svc.Release),
		len(svc.Fqdn),
		len(svc.Application)+len(svc.Environment)+1,
		len(statusLabelFor(svc)),
	)

	fmt.Fprintf(os.Stderr, fmtString, "id", "svc.group", "release", "FQDN", "app:env", "status")
	for _, svc := range res.Services {
		fmt.Printf("%s\t%s\t%s\t%s\t%s:%s\t%s\n",
			svc.Id,
			svc.Group,
			svc.Release,
			svc.Fqdn,
			svc.Application, svc.Environment,
			statusLabelFor(svc),
		)
	}

	return nil
}

func statusLabelFor(s *applications.Service) string {
	if s.Disconnected {
		return "DISCONNECTED"
	} else {
		return string(s.HealthCheck)
	}
}
