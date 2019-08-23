package server

import (
	"context"
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/components/compliance-service/api/stats"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/chef/automate/lib/errorutils"
)

// Server implementation for stats
type Server struct {
	es *relaxting.ES2Backend
}

// New creates a new server
func New(es *relaxting.ES2Backend) *Server {
	return &Server{es: es}
}

// ReadSummary returns summary, nodes-summary, or controls-summary information
func (srv *Server) ReadSummary(ctx context.Context, in *stats.Query) (*stats.Summary, error) {
	var summary stats.Summary

	formattedFilters, err := relaxting.FilterByProjects(ctx, formatFilters(in.Filters))
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	if in.Type == "" {
		reportSummary, err := srv.es.GetStatsSummary(formattedFilters)
		if err != nil {
			err = errorutils.FormatErrorMsg(err, "")
			return nil, err
		}
		summary.ReportSummary = reportSummary
	}
	if in.Type == "nodes" {
		nodeSummary, err := srv.es.GetStatsSummaryNodes(formattedFilters)
		if err != nil {
			err = errorutils.FormatErrorMsg(err, "")
			return nil, err
		}
		summary.NodeSummary = nodeSummary
	}
	if in.Type == "controls" {
		controlSummary, err := srv.es.GetStatsSummaryControls(formattedFilters)
		if err != nil {
			err = errorutils.FormatErrorMsg(err, "")
			return nil, err
		}
		summary.ControlsSummary = controlSummary
	}
	return &summary, nil
}

// ReadTrend returns trend information for nodes or controls
func (srv *Server) ReadTrend(ctx context.Context, in *stats.Query) (*stats.Trends, error) {
	var trends stats.Trends
	var trend []*stats.Trend
	formattedFilters, err := relaxting.FilterByProjects(ctx, formatFilters(in.Filters))
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	// set a default interval here if it is 0. This is done here instead
	// of in the validateTrendData function b/c it is a nicety that modifies
	// the value of the query sent in. The rest of the validateTrendData checks
	// only error out when invalid.
	if in.Interval == 0 {
		in.Interval = 86400
	}
	err = validateTrendData(in, formattedFilters)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}

	trend, err = srv.es.GetTrend(formattedFilters, int(in.Interval), in.Type)
	if err != nil {
		err = errorutils.FormatErrorMsg(err, "")
		return nil, err
	}

	trends.Trends = trend
	return &trends, nil
}

// ReadProfiles returns profile stats information (generalized and per profile-id)
func (srv *Server) ReadProfiles(ctx context.Context, in *stats.Query) (*stats.Profile, error) {
	var profile stats.Profile
	if in.Size == 0 {
		in.Size = 10000
	}

	formattedFilters, err := relaxting.FilterByProjects(ctx, formatFilters(in.Filters))
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	if in.Id != "" {
		if in.Type == "" {
			return nil, status.Error(codes.InvalidArgument, fmt.Sprintf("Type must be specified when requesting specific profile information. Please specify type 'summary' or 'controls'"))
		}
		if in.Type == "summary" {
			profileSummary, err := srv.es.GetProfileSummaryByProfileId(in.Id, formattedFilters)
			if err != nil {
				err = errorutils.FormatErrorMsg(err, "")
				return nil, err
			}
			profile.ProfileSummary = profileSummary
		}
		if in.Type == "controls" {
			from, perPage, sort, order, err := validatePaginationAndSorting(in)
			if err != nil {
				return nil, status.Error(codes.InvalidArgument, err.Error())
			}
			controlStats, err := srv.es.GetControlListStatsByProfileID(in.Id, int(from), int(perPage), formattedFilters, sort, order)
			if err != nil {
				err = errorutils.FormatErrorMsg(err, "")
				return nil, err
			}
			profile.ControlStats = controlStats
		}
	} else {
		profileList, err := srv.es.GetProfileListWithAggregatedComplianceSummaries(formattedFilters, in.Size)
		if err != nil {
			err = errorutils.FormatErrorMsg(err, "")
			return nil, err
		}
		profile.ProfileList = profileList
	}
	return &profile, nil
}

// ReadFailures returns failures by platform, environment, etc
func (srv *Server) ReadFailures(ctx context.Context, in *stats.Query) (*stats.Failures, error) {
	formattedFilters, err := relaxting.FilterByProjects(ctx, formatFilters(in.Filters))
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	// i went back and forth on this one for a while. while i see
	// the reason it could be its own field in the query message type,
	// it also seems fitting to stick it in as a filter...and I don't
	// like the idea of having a type and types in that msg...and I don't
	// like the idea of making type an array format all the time...urhhhhhhhhhh
	// i dunno :/
	if len(formattedFilters["types"]) == 0 {
		return nil, status.Error(codes.InvalidArgument, fmt.Sprintf("A filter of value 'types' must be specified. Valid values are 'platform', 'environment', 'control', and 'profile'"))
	}
	if in.Size == 0 {
		in.Size = 10
	}
	failures, err := srv.es.GetStatsFailures(formattedFilters["types"], int(in.Size), formattedFilters)
	if err != nil {
		err = errorutils.FormatErrorMsg(err, "")
		return nil, err
	}
	return failures, nil
}

func validateTrendData(in *stats.Query, filters map[string][]string) (err error) {
	if in.Type == "" {
		err = fmt.Errorf("Please specify the type of trend data you are requesting; nodes or controls")
		return
	}
	if in.Interval < 3600 {
		err = fmt.Errorf("Minimum value for 'interval' is 3600(one hour)")
		return
	}
	if len(filters["end_time"]) == 0 {
		err = fmt.Errorf("Required filter 'end_time' must be a date with format: 'YYYY-MM-DDThh:mm:ss+|-hh:mm'")
		return
	}
	if len(filters["start_time"]) == 0 {
		err = fmt.Errorf("Required filter 'start_time' must be a date with format: 'YYYY-MM-DDThh:mm:ss+|-hh:mm'")
		return
	}
	return nil
}

func validatePaginationAndSorting(in *stats.Query) (from int32, perPage int32, sort string, asc bool, err error) {
	if in.PerPage == 0 {
		in.PerPage = 1000
	}
	perPage = in.PerPage

	if in.Page == 0 {
		in.Page = 1
	}
	from = (in.Page - 1) * in.PerPage

	if in.Order == 0 {
		asc = true
	} else {
		asc = false
	}
	if in.Sort != "" {
		if in.Sort != "control-name" {
			err = fmt.Errorf("Invalid argument: Valid sort fields are control-name")
			return
		}
		sort = "control-name"
	} else {
		sort = "latest_report.end_time"
	}
	return
}

func formatFilters(filters []*stats.ListFilter) map[string][]string {
	formattedFilters := make(map[string][]string)
	for _, filter := range filters {
		formattedFilters[filter.Type] = filter.Values
	}
	return formattedFilters
}
