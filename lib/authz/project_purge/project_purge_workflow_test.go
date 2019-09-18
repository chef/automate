package project_purge

import (
	"math"
	"testing"
	"time"

	"github.com/chef/automate/lib/cereal/cerealtest"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

const (
	svcName       = "testSvc"
	testProjectID = "testproject"
	startTaskName = "testSvc/StartPurgeProjects"
)

func TestWorkflowOnTaskComplete(t *testing.T) {
	workflowParams := DomainProjectPurgeWorkflowParameters{
		ProjectID: testProjectID,
	}
	executor := NewWorkflowExecutorForDomainService(svcName)

	t.Run("when startPurgeProjectTaskName completes", func(t *testing.T) {
		curPayload := DomainProjectPurgeWorkflowPayload{}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(workflowParams).
			WithPayload(curPayload)

		tr := cerealtest.NewTaskResult(t)
		ev := cerealtest.NewTaskCompleteEvent(startTaskName, tr)
		executor.OnTaskComplete(instance, ev)
		instance.AssertComplete()
	})
	t.Run("when startPurgeProjectTaskName fails within max time bound it exponentially retries", func(t *testing.T) {
		numberOfFailures := 5
		expectedTime := time.Now().Add(time.Duration(math.Pow(float64(numberOfFailures), 2.0)) * time.Second)
		failureErr := errors.New("fail")
		curPayload := DomainProjectPurgeWorkflowPayload{ConsecutiveJobCheckFailures: numberOfFailures - 1}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(workflowParams).
			WithPayload(curPayload)

		tr := cerealtest.NewTaskResult(t).WithError(failureErr)
		ev := cerealtest.NewTaskCompleteEvent(startTaskName, tr)
		executor.OnTaskComplete(instance, ev)
		assertEnqueued := instance.AssertContinuing().AssertTaskEnqueued(startTaskName)
		assertEnqueued.AssertCount(1)
		params := DomainProjectPurgeWorkflowParameters{}
		assertEnqueued.Tasks[0].GetParameters(&params)
		assert.Equal(t, testProjectID, params.ProjectID)
		assert.True(t, assertEnqueued.Tasks[0].Opts.StartAfter.After(expectedTime))
	})
	t.Run("when startPurgeProjectTaskName fails outside max time bound it uses the max allowed", func(t *testing.T) {
		numberOfFailures := 10
		expectedTime := time.Now().Add(maxBackoffDuration)
		failureErr := errors.New("fail")
		curPayload := DomainProjectPurgeWorkflowPayload{ConsecutiveJobCheckFailures: numberOfFailures - 1}
		instance := cerealtest.
			NewWorkflowInstance(t, "instanceName").
			WithParameters(workflowParams).
			WithPayload(curPayload)

		tr := cerealtest.NewTaskResult(t).WithError(failureErr)
		ev := cerealtest.NewTaskCompleteEvent(startTaskName, tr)
		executor.OnTaskComplete(instance, ev)
		assertEnqueued := instance.AssertContinuing().AssertTaskEnqueued(startTaskName)
		assertEnqueued.AssertCount(1)
		params := DomainProjectPurgeWorkflowParameters{}
		assertEnqueued.Tasks[0].GetParameters(&params)
		assert.Equal(t, testProjectID, params.ProjectID)
		assert.True(t, assertEnqueued.Tasks[0].Opts.StartAfter.After(expectedTime))
	})
}
