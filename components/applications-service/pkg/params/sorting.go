package params

import (
	"github.com/chef/automate/api/external/common/query"
	"github.com/pkg/errors"
)

// SortField defines our valid sorting fields
type SortField int

const (
	DefaultSortFieldForServiceGroups = "percent_ok"
	DefaultSortFieldForServices      = "health"
	DefaultSortAscending             = true

	// Define here your sort fields
	DefaultField SortField = iota
	PercentOkField
	NameField
	HealthField
	AppField
	EnvironmentField
)

var (
	// Add the defined sort fields to the right mapping
	validSortFieldsForServiceGroups = map[string]SortField{
		"name":        NameField,
		"percent_ok":  PercentOkField,
		"health":      HealthField,
		"app_name":    AppField,
		"environment": EnvironmentField,
		"":            DefaultField,
	}
	validSortFieldsForServices = map[string]SortField{
		"health": HealthField,
		"":       DefaultField,
	}
)

// GetSortParamsForServices returns valid sorting parameters for services from the sort query
func GetSortParamsForServices(s *query.Sorting) (sortField string, sortAsc bool, err error) {
	sortField = DefaultSortFieldForServices
	sortAsc = DefaultSortAscending

	if !isValidSortFieldForServices(s) {
		err = errors.Errorf("Invalid sort field '%s'.", s.GetField())
		return
	}

	if s.GetField() != "" {
		sortField = s.Field
	}

	if s.GetOrder().String() == "DESC" {
		sortAsc = false
	}

	return
}

// GetSortParamsForServiceGroups returns valid sorting parameters for service_groups from the sort query
func GetSortParamsForServiceGroups(s *query.Sorting) (sortField string, sortAsc bool, err error) {
	sortField = DefaultSortFieldForServiceGroups
	sortAsc = DefaultSortAscending

	if !isValidSortFieldForServiceGroups(s) {
		err = errors.Errorf("Invalid sort field '%s'.", s.GetField())
		return
	}

	if s.GetField() != "" {
		sortField = s.Field
	}

	if s.GetOrder().String() == "DESC" {
		sortAsc = false
	}

	return
}

func isValidSortFieldForServices(s *query.Sorting) bool {
	if _, ok := validSortFieldsForServices[s.GetField()]; ok {
		return true
	}
	return false
}

func isValidSortFieldForServiceGroups(s *query.Sorting) bool {
	if _, ok := validSortFieldsForServiceGroups[s.GetField()]; ok {
		return true
	}
	return false
}
