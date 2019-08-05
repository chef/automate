package builder

import (
	"errors"
	"fmt"
	"strings"
	"time"

	chef "github.com/chef/automate/api/external/ingest/request"
	. "github.com/chef/automate/components/notifications-client/api"
	_struct "github.com/golang/protobuf/ptypes/struct"
)

// ChefClientConverge builds a chef converge event that can be sent to the notifier
// url is the base automate url
// TODO: url is the url where we can find more information about the event
func ChefClientConverge(url string, run *chef.Run) (*Event, error) {
	var ev Event
	ev.Id = run.Id

	if run.Status == "failure" {
		failed_resource := getFailedResources(run.GetResources())
		if failed_resource == nil {
			return nil, errors.New("Could not find failed resource in chef run")
		}

		err := run.GetError()
		if err == nil {
			return nil, errors.New("Could not find error struct in chef run")
		}

		err_desc := err.GetDescription()
		if err == nil {
			return nil, errors.New("Could not find description of error struct in chef run")
		}

		ccr_failure := CCRFailure{
			RunId:    run.GetRunId(),
			NodeName: run.GetNodeName(),
			Cookbook: failed_resource.GetCookbookName(),
			Recipe:   recipe(failed_resource.GetRecipeName()),
			Time: &TimeInfo{
				// TODO: We should at the very least have a canonical timestamp representation
				StartTime: run.GetStartTime(),
				EndTime:   run.GetEndTime(),
			},
			Exception: &ExceptionInfo{
				Class:     err.GetClass(),
				Title:     err_desc.GetTitle(),
				Msg:       err.GetMessage(),
				Backtrace: err.GetBacktrace(),
			},
			Timestamp: time.Now().Format(time.RFC3339),
			NodeUrl:   run.GetChefServerFqdn(),
			RunUrl:    runUrlFor(url, run.GetEntityUuid(), run.GetRunId()),
		}

		ev.Event = &Event_CCRFailure{&ccr_failure}
	} else {
		ccr_success := CCRSuccess{
			RunId:    run.GetId(),
			NodeName: run.GetNodeName(),
			RunUrl:   runUrlFor(url, run.GetEntityUuid(), run.GetRunId()),
			Time: &TimeInfo{
				StartTime: run.GetStartTime(),
				EndTime:   run.GetEndTime(),
			},
			UpdatedResourceCount: run.GetUpdatedResourceCount(),
			Timestamp:            time.Now().Format(time.RFC3339),
		}

		ev.Event = &Event_CCRSuccess{&ccr_success}
	}

	return &ev, nil
}

func getFailedResources(resources []*chef.Resource) *chef.Resource {
	for _, resource := range resources {
		ignoreFailure := resource.GetIgnoreFailure()
		if x, ok := ignoreFailure.GetKind().(*_struct.Value_BoolValue); ok && !x.BoolValue && resource.GetStatus() == "failed" {
			return resource
		}
	}
	return nil
}

func recipe(r string) string {
	if strings.TrimSpace(r) == "" {
		return "default"
	}
	return r
}

//TODO: move to gateway
func runUrlFor(base_url string, entity_uuid string, run_id string) string {
	return fmt.Sprintf("%s/infrastructure/client-runs/%s/runs/%s", base_url, entity_uuid, run_id)
}
