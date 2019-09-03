package integration_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/habitat"
	"github.com/golang/protobuf/proto"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDistinctValuesWithNoFiltersApplied(t *testing.T) {
	suite.DeleteDataFromStorage()
	defer suite.DeleteDataFromStorage()

	var (
		eventsProcessed = suite.Ingester.EventsProcessed()
		eventAAA        = NewHabitatEvent(
			withSupervisorId("1111"),
			withPackageIdent("aaa-origin/aaa-service_name/1.0.0/aaa-buildstamp"),
			withServiceGroup("aaa-service_name.aaa-group_name"),
			withStrategyAtOnce("aaa-channel"),
			withApplication("aaa-application"),
			withEnvironment("aaa-environment"),
			withFqdn("aaa.example.com"),
			withHealth(HealthCheckIntToString(0)), // -> OK
			withSite("aaa-site"),
		)
		eventAAB = NewHabitatEvent(
			withSupervisorId("222"),
			withPackageIdent("aab-origin/aab-service_name/1.0.1/aab-buildstamp"),
			withServiceGroup("aab-service_name.aab-group_name"),
			withStrategyAtOnce("aab-channel"),
			withApplication("aab-application"),
			withEnvironment("aab-environment"),
			withFqdn("aab.example.com"),
			withHealth(HealthCheckIntToString(0)), // -> OK
			withSite("aab-site"),
		)
		eventABB = NewHabitatEvent(
			withSupervisorId("333"),
			withPackageIdent("abb-origin/abb-service_name/1.1.1/abb-buildstamp"),
			withServiceGroup("abb-service_name.abb-group_name"),
			withStrategyAtOnce("abb-channel"),
			withApplication("abb-application"),
			withEnvironment("abb-environment"),
			withFqdn("abb.example.com"),
			withHealth(HealthCheckIntToString(0)), // -> OK
			withSite("abb-site"),
		)
	)
	allEvents := []*habitat.HealthCheckEvent{
		eventAAA,
		eventAAB,
		eventABB,
	}

	for _, e := range allEvents {
		bytes, err := proto.Marshal(e)
		require.NoError(t, err)
		suite.Ingester.IngestMessage(bytes)
		eventsProcessed++
	}

	suite.WaitForEventsToProcess(eventsProcessed)

	svcList := suite.GetServices()
	require.Equal(t, 3, len(svcList))

	scenarioForField(t, "service_name")
	scenarioForField(t, "origin")
	scenarioForField(t, "channel")
	scenarioForField(t, "application")
	scenarioForField(t, "environment")
	scenarioForField(t, "site")
	scenarioForField(t, "group_name")
	scenarioForField(t, "buildstamp")

	t.Run("distinct values filtering for field version", func(t *testing.T) {
		fieldName := "version"

		expectedValueAAA := "1.0.0"
		expectedValueAAB := "1.0.1"
		expectedValueABB := "1.1.1"

		queryFragment := ""
		actual := getDistinctValues(t, fieldName, queryFragment)
		expected := []string{
			expectedValueAAA,
			expectedValueAAB,
			expectedValueABB,
		}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)

		queryFragment = "1"
		actual = getDistinctValues(t, fieldName, queryFragment)
		expected = []string{
			expectedValueAAA,
			expectedValueAAB,
			expectedValueABB,
		}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)

		queryFragment = "1.0"
		actual = getDistinctValues(t, fieldName, queryFragment)
		expected = []string{
			expectedValueAAA,
			expectedValueAAB,
		}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)

		queryFragment = "1.0.0"
		actual = getDistinctValues(t, fieldName, queryFragment)
		expected = []string{
			expectedValueAAA,
		}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)

		queryFragment = "2.0.0"
		actual = getDistinctValues(t, fieldName, queryFragment)
		expected = []string{}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)
	})
}

func scenarioForField(t *testing.T, fieldName string) {
	scenarioName := fmt.Sprintf("distinct values filtering for field %s", fieldName)

	expectedValueAAA := fmt.Sprintf("aaa-%s", fieldName)
	expectedValueAAB := fmt.Sprintf("aab-%s", fieldName)
	expectedValueABB := fmt.Sprintf("abb-%s", fieldName)

	t.Run(scenarioName, func(t *testing.T) {
		// Empty queryFragment returns all the values
		queryFragment := ""
		actual := getDistinctValues(t, fieldName, queryFragment)
		expected := []string{
			expectedValueAAA,
			expectedValueAAB,
			expectedValueABB,
		}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)

		// A queryFragment that matches all the values returns all the values
		queryFragment = "a"
		actual = getDistinctValues(t, fieldName, queryFragment)
		expected = []string{
			expectedValueAAA,
			expectedValueAAB,
			expectedValueABB,
		}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)

		// A queryFragment that matches some values returns the matching subset
		queryFragment = "aa"
		actual = getDistinctValues(t, fieldName, queryFragment)
		expected = []string{
			expectedValueAAA,
			expectedValueAAB,
		}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)

		// A queryFragment that matches one value returns the one that matches
		queryFragment = "aaa"
		actual = getDistinctValues(t, fieldName, queryFragment)
		expected = []string{
			expectedValueAAA,
		}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)

		// A queryFragment that matches nothing returns an empty set
		queryFragment = "aaaa"
		actual = getDistinctValues(t, fieldName, queryFragment)
		expected = []string{}
		assert.Equalf(t, expected, actual, "GetServicesDistinctValues for %q with queryFragment %q", fieldName, queryFragment)
	})
}

func getDistinctValues(t *testing.T, fieldName, queryFragment string) []string {
	ctx := context.Background()
	request := &applications.ServicesDistinctValuesReq{FieldName: fieldName, QueryFragment: queryFragment}
	response, err := suite.ApplicationsServer.GetServicesDistinctValues(ctx, request)
	require.NoError(t, err)
	return response.Values
}
