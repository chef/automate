package elasticutil

import (
	"context"
	"fmt"

	"github.com/pkg/errors"
	elastic "gopkg.in/olivere/elastic.v6"
)

type UpdateByQueryTaskResponse struct {
	TimedOut bool `json:"timed_out"`
	Retries  struct {
		Bulk   int `json:"bulk"`
		Search int `json:"search"`
	} `json:"retries"`
	Total            int      `json:"total"`
	Updated          int      `json:"updated"`
	Created          int      `json:"created"`
	Deleted          int      `json:"deleted"`
	Batches          int      `json:"batches"`
	VersionConflicts int      `json:"version_conflicts"`
	Noops            int      `json:"noops"`
	Failures         []string `json:"failures"`
}

type UpdateByQueryTaskStatus struct {
	Retries struct {
		Bulk   int `json:"bulk"`
		Search int `json:"search"`
	} `json:"retries"`
	Total            int `json:"total"`
	Updated          int `json:"updated"`
	Created          int `json:"created"`
	Deleted          int `json:"deleted"`
	Batches          int `json:"batches"`
	VersionConflicts int `json:"version_conflicts"`
	Noops            int `json:"noops"`
}

type UpdateByQueryTaskInfo struct {
	Node               string                   `json:"node"`
	Id                 int64                    `json:"id"` // the task id (yes, this is a long in the Java source)
	Type               string                   `json:"type"`
	Action             string                   `json:"action"`
	Status             *UpdateByQueryTaskStatus `json:"status"`      // has separate implementations of Task.Status in Java for reindexing, replication, and "RawTaskStatus"
	Description        interface{}              `json:"description"` // same as Status
	StartTimeInMillis  int64                    `json:"start_time_in_millis"`
	RunningTimeInNanos int64                    `json:"running_time_in_nanos"`
	Cancellable        bool                     `json:"cancellable"`
	ParentTaskId       string                   `json:"parent_task_id"` // like "YxJnVYjwSBm_AUbzddTajQ:12356"
	Headers            map[string]string        `json:"headers"`
}

type UpdateByQueryTask struct {
	Completed bool                       `json:"completed"`
	Task      *UpdateByQueryTaskInfo     `json:"task,omitempty"`
	Response  *UpdateByQueryTaskResponse `json:"response"`
}

func GetUpdateByQueryTask(ctx context.Context, client *elastic.Client, taskID string) (*UpdateByQueryTask, error) {
	path := fmt.Sprintf("/_tasks/%s", taskID)
	res, err := client.PerformRequest(ctx, elastic.PerformRequestOptions{
		Method: "GET",
		Path:   path,
	})

	if err != nil {
		return nil, err
	}

	if res.StatusCode != 200 {
		return nil, errors.Errorf("GET %s failed with status %d: %s", path, res.StatusCode, res.Body)
	}

	return decodeGetUpdateByQueryTaskResp(res.Body)
}

func decodeGetUpdateByQueryTaskResp(body []byte) (*UpdateByQueryTask, error) {
	ret := new(UpdateByQueryTask)
	decoder := &elastic.DefaultDecoder{}
	if err := decoder.Decode(body, ret); err != nil {
		return nil, err
	}

	return ret, nil
}
