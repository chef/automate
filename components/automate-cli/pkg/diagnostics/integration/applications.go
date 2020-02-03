package integration

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"strings"
	"time"

	"github.com/golang/protobuf/proto"
	w "github.com/golang/protobuf/ptypes/wrappers"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/external/habitat"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const (
	// Hard code the length here so we can't have a bug where we create zero
	// services and don't verify anything.
	expectedServicesCount = 4
)

type AppsDiagnosticsRun struct {
	ServiceGroupSuffix string
	Events             []*habitat.HealthCheckEvent
	// For applications testing, the determination of whether to skip the test is
	// based on (more or less) the version of automate. Some Ci scenarios
	// generate data with older automate and then verify it with a newer
	// automate. The test framework does a fresh skip check for each one, which
	// can lead to the verify step being run after the generate step was skipped.
	// To handle that, we persist the skipped state
	Skipped bool
}

type ServicesResponse struct {
	Services []ServiceResponse `json:"services"`
}

// ServiceResponse is a struct that matches the responses returned by the
// HTTP/JSON API. It omits a few fields that are controlled by the server since
// they aren't relevant for the kind of testing we are doing here.
//
// An individual service object in a response currently looks like this:
//
//   ```
//   {
//     "supervisor_id": "44444444-4444-4444-4444-444444444444",
//     "release": "test/db/4.4.4/20200101121211",
//     "group": "db.diagnostics-run-dc1b29fcc068f7478e6cfce7f63f0ac28c90cae607170fdeb1a21f01738774fd",
//     "health_check": "UNKNOWN",
//     "application": "test-app",
//     "environment": "test-env-4",
//     "fqdn": "diagnostics-4.example",
//     "channel": "stable",
//     "update_strategy": "AT-ONCE",
//     "site": "test-site-4",
//     "previous_health_check": "NONE",
//     "current_health_since": "2 days 19 hours",
//     "health_updated_at": "2020-01-24T22:59:22.199956Z",
//     "disconnected": true,
//     "last_event_occurred_at": "2020-01-25T00:13:32.629672Z",
//     "last_event_since": "2 days 18 hours",
//     "health_check_result": {
//         "stdout": "example stdout",
//         "stderr": "example stderr",
//         "exit_status": 0
//     },
//     "id": "1304"
//   }
//   ```
//
type ServiceResponse struct {
	ID                string              `json:"id"`
	SupID             string              `json:"supervisor_id"`
	Release           string              `json:"release"`
	HealthCheck       string              `json:"health_check"`
	Application       string              `json:"application"`
	Environment       string              `json:"environment"`
	FQDN              string              `json:"fqdn"`
	Channel           string              `json:"channel"`
	Site              string              `json:"site"`
	HealthCheckResult HealthCheckResponse `json:"health_check_result"`
}

type HealthCheckResponse struct {
	Stdout     string `json:"stdout"`
	Stderr     string `json:"stderr"`
	ExitStatus int32  `json:"exit_status"`
}

func CreateApplicationsDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "applications-services",
		// "deep upgrades" test upgrading from the earliest versions of automate.
		// Applications functionality didn't exist then so we skip those
		Tags:     diagnostics.Tags{"applications", "skip-for-deep-upgrade"},
		Skip:     AppsCheckSkip,
		Generate: AppsLoadData,
		Verify:   AppsVerifyData,
		Cleanup:  AppsCleanupData,
	}
}

func AppsCheckSkip(testCtx diagnostics.TestContext) (bool, string, error) {
	ok, err := serverNewEnoughForAppsTest(testCtx)
	if err != nil {
		return false, "", err
	}
	if ok {
		return false, "", nil
	}
	runData := &AppsDiagnosticsRun{Skipped: true}
	testCtx.SetValue("applications", runData)
	return true, "target server does not support all required applications APIs", nil
}

func AppsLoadData(testCtx diagnostics.TestContext) error {
	randomID := make([]byte, 32)
	_, err := rand.Read(randomID)
	if err != nil {
		return errors.Wrap(err, "failed to generate random ID for applications test data")
	}

	runData := &AppsDiagnosticsRun{}

	testTag := hex.EncodeToString(randomID)
	serviceGroupSuffix := fmt.Sprintf("diagnostics-run-%s", testTag)
	serviceGroup := fmt.Sprintf("db.%s", serviceGroupSuffix)

	runData.ServiceGroupSuffix = serviceGroupSuffix
	testCtx.SetValue("applications", runData)

	events := makeDataSet(serviceGroup)
	messages := make([][]byte, len(events))
	for i, event := range events {
		b, err := proto.Marshal(event)
		if err != nil {
			return err
		}

		messages[i] = b
	}

	err = testCtx.PublishViaNATS(messages)
	if err != nil {
		return errors.Wrap(err, "failed to publish applications messages to Automate")
	}

	runData.Events = events
	testCtx.SetValue("applications", runData)

	return nil
}

func AppsVerifyData(verifyCtx diagnostics.VerificationTestContext) {
	runData := AppsDiagnosticsRun{}
	err := verifyCtx.GetValue("applications", &runData)
	if err != nil {
		verifyCtx.Errorf("No applications data in test context")
		verifyCtx.FailNow()
	}

	if runData.Skipped {
		return
	}

	services, err := getServicesWithIngestDelay(verifyCtx, &runData)
	if err != nil {
		verifyCtx.Errorf("API did not return a valid response for Applications data: %s", err.Error())
		verifyCtx.FailNow()
	}

	assert.Len(verifyCtx, services.Services, expectedServicesCount)

	for _, event := range runData.Events {
		assertEventIsInResponse(verifyCtx, services, event)
	}

}

func AppsCleanupData(testCtx diagnostics.TestContext) error {
	runData := AppsDiagnosticsRun{}
	err := testCtx.GetValue("applications", &runData)
	if err != nil {
		return errors.Wrap(err, "failed to load run data")
	}

	if runData.Skipped {
		return nil
	}

	services, err := getServicesWithIngestDelay(testCtx, &runData)
	if err != nil {
		return errors.Wrap(err, "API did not return a valid response for Applications data")
	}
	serviceIDs := make([]string, len(services.Services))

	for i, svc := range services.Services {
		serviceIDs[i] = svc.ID
	}

	reqPath := "/api/v0/applications/delete_services_by_id"
	reqBodyfmt := `
{"ids": [ "%s" ]}
`
	reqBody := fmt.Sprintf(reqBodyfmt, strings.Join(serviceIDs, `", "`))

	maxTries := 5
	for tries := 0; tries < maxTries; tries++ {
		resp, err := testCtx.DoLBRequest(reqPath,
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONBody(reqBody),
		)
		resp.Body.Close()

		if err == nil {
			break
		}

		time.Sleep(2 * time.Duration(tries) * time.Second)
	}
	return err
}

func makeDataSet(serviceGroup string) []*habitat.HealthCheckEvent {
	return []*habitat.HealthCheckEvent{
		{
			EventMetadata: &habitat.EventMetadata{
				SupervisorId: "11111111-1111-1111-1111-111111111111",
				Fqdn:         "diagnostics.example",
				Site:         "test-site-1",
				Application:  "test-app",
				Environment:  "test-env-1",
			},
			ServiceMetadata: &habitat.ServiceMetadata{
				PackageIdent: "test/db/1.1.1/20200101121211",
				ServiceGroup: serviceGroup,
				UpdateConfig: &habitat.UpdateConfig{
					Strategy: habitat.UpdateStrategy_AtOnce,
					Channel:  "stable",
				},
			},
			Result: habitat.HealthCheckResult(0),
			Stdout: wString("example stdout"),
			Stderr: wString("example stderr"),
		},
		{
			EventMetadata: &habitat.EventMetadata{
				SupervisorId: "22222222-2222-2222-2222-222222222222",
				Fqdn:         "diagnostics-2.example",
				Site:         "test-site-2",
				Application:  "test-app",
				Environment:  "test-env-2",
			},
			ServiceMetadata: &habitat.ServiceMetadata{
				PackageIdent: "test/db/2.2.2/20200101121211",
				ServiceGroup: serviceGroup,
				UpdateConfig: &habitat.UpdateConfig{
					Strategy: habitat.UpdateStrategy_AtOnce,
					Channel:  "stable",
				},
			},
			Result: habitat.HealthCheckResult(1),
			Stdout: wString("example stdout"),
			Stderr: wString("example stderr"),
		},
		{
			EventMetadata: &habitat.EventMetadata{
				SupervisorId: "33333333-3333-3333-3333-333333333333",
				Fqdn:         "diagnostics-3.example",
				Site:         "test-site-3",
				Application:  "test-app",
				Environment:  "test-env-3",
			},
			ServiceMetadata: &habitat.ServiceMetadata{
				PackageIdent: "test/db/3.3.3/20200101121211",
				ServiceGroup: serviceGroup,
				UpdateConfig: &habitat.UpdateConfig{
					Strategy: habitat.UpdateStrategy_AtOnce,
					Channel:  "stable",
				},
			},
			Result: habitat.HealthCheckResult(2),
			Stdout: wString("example stdout"),
			Stderr: wString("example stderr"),
		},
		{
			EventMetadata: &habitat.EventMetadata{
				SupervisorId: "44444444-4444-4444-4444-444444444444",
				Fqdn:         "diagnostics-4.example",
				Site:         "test-site-4",
				Application:  "test-app",
				Environment:  "test-env-4",
			},
			ServiceMetadata: &habitat.ServiceMetadata{
				PackageIdent: "test/db/4.4.4/20200101121211",
				ServiceGroup: serviceGroup,
				UpdateConfig: &habitat.UpdateConfig{
					Strategy: habitat.UpdateStrategy_AtOnce,
					Channel:  "stable",
				},
			},
			Result: habitat.HealthCheckResult(3),
			Stdout: wString("example stdout"),
			Stderr: wString("example stderr"),
		},
	}
}

// assertEventIsInResponse loops through the services in the ServicesResponse
// to find the given event and then checks that the values of the object's
// properties are as expected. It treats the SupervisorId as a unique key which
// is true of our generated data but not enforced by the data model.
func assertEventIsInResponse(verifyCtx diagnostics.VerificationTestContext, services *ServicesResponse, event *habitat.HealthCheckEvent) {
	var actual *ServiceResponse
	targetSupId := event.EventMetadata.SupervisorId
	for _, svc := range services.Services {
		if svc.SupID == targetSupId {
			actual = &svc
			break
		}
	}
	if actual == nil {
		verifyCtx.Errorf("Unable to find event with supervisor_id %s in the response", targetSupId)
		verifyCtx.FailNow()
	}

	e := event.EventMetadata
	s := event.ServiceMetadata

	assert.Equal(verifyCtx, s.PackageIdent, actual.Release)
	assert.Equal(verifyCtx, e.Application, actual.Application)
	assert.Equal(verifyCtx, e.Environment, actual.Environment)
	assert.Equal(verifyCtx, e.Fqdn, actual.FQDN)
	assert.Equal(verifyCtx, s.UpdateConfig.Channel, actual.Channel)
	assert.Equal(verifyCtx, e.Site, actual.Site)
	assert.Equal(verifyCtx, expectedHCString(event.Result), actual.HealthCheck)
	assert.Equal(verifyCtx, event.Stdout.Value, actual.HealthCheckResult.Stdout)
	assert.Equal(verifyCtx, event.Stderr.Value, actual.HealthCheckResult.Stderr)
}

// serverNewEnough determines if the automate server can run this test. The
// most recently added API we require is applications/delete_services_by_id so
// that's what we look for
func serverNewEnoughForAppsTest(ctx diagnostics.TestContext) (bool, error) {
	reqPath := "/api/v0/applications/delete_services_by_id"
	reqBody := `{"ids": [ ]}`

	resp, err := ctx.DoLBRequest(reqPath,
		lbrequest.WithMethod("POST"),
		lbrequest.WithJSONBody(reqBody),
	)
	if err != nil {
		return false, errors.Wrap(err, "failed API request attempting to determine server capabilities")
	}
	resp.Body.Close()

	if resp.StatusCode == 404 {
		return false, nil
	}

	return true, nil
}

// getServicesWithIngestDelay requests the services data from the API and
// returns it as a ServicesResponse. It will retry up to 5 times if the
// response doesn't have `expectedServicesCount` services. After that it will
// return what it got from the last request and it WILL NOT error or fail. This
// behavior makes getServicesWithIngestDelay able to do a best effort attempt
// to find the services to delete in the AppsCleanupData step.
func getServicesWithIngestDelay(ctx diagnostics.TestContext, runData *AppsDiagnosticsRun) (*ServicesResponse, error) {
	var services *ServicesResponse
	var err error

	// Give the server some time to ingest everything
	maxTries := 5
	for tries := 0; tries < maxTries; tries++ {
		services, err = getServicesForRun(ctx, runData)
		if err != nil {
			return nil, err
		}

		if len(services.Services) == expectedServicesCount {
			break
		}
		time.Sleep(2 * time.Duration(tries) * time.Second)
	}
	return services, nil
}

func getServicesForRun(ctx diagnostics.TestContext, runData *AppsDiagnosticsRun) (*ServicesResponse, error) {
	reqPath := fmt.Sprintf("/api/v0/applications/services?filter=group:%s", runData.ServiceGroupSuffix)
	resp, err := ctx.DoLBRequest(reqPath)
	if err != nil {
		return nil, errors.Wrap(err, "API request for applications data failed")
	}
	defer resp.Body.Close()

	response := ServicesResponse{}
	err = json.NewDecoder(resp.Body).Decode(&response)

	if err != nil {
		return nil, errors.Wrap(err, "API request for applications data failed to return usable data")
	}

	return &response, nil
}

func expectedHCString(hc habitat.HealthCheckResult) string {
	switch hc {
	case habitat.HealthCheckResult(0):
		return "OK"
	case habitat.HealthCheckResult(1):
		return "WARNING"
	case habitat.HealthCheckResult(2):
		return "CRITICAL"
	case habitat.HealthCheckResult(3):
		return "UNKNOWN"
	default:
		return fmt.Sprintf("out of bounds HealthCheckResult %+v", hc)
	}

}

func wString(s string) *w.StringValue {
	return &w.StringValue{Value: s}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateApplicationsDiagnostic(),
	)
}
