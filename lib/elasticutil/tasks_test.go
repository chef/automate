package elasticutil

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestJSONDecodeUpdateByQueryTask(t *testing.T) {
	jsonData := `
	{
		"completed": true,
		"task": {
		  "node": "6kynW36yQJG6Kd2qFpJ6YA",
		  "id": 170364,
		  "type": "transport",
		  "action": "indices:data/write/update/byquery",
		  "status": {
			"total": 100,
			"updated": 1,
			"created": 2,
			"deleted": 3,
			"batches": 4,
			"version_conflicts": 5,
			"noops": 6,
			"retries": {
			  "bulk": 7,
			  "search": 8
			},
			"throttled_millis": 1,
			"requests_per_second": -1,
			"throttled_until_millis": 3
		  },
		  "start_time_in_millis": 1583092186694,
		  "running_time_in_nanos": 32874403,
		  "headers": {
			  "X-Opaque-Id": "123456"
		  }
		},
		"response": {
		  "took": 32,
		  "timed_out": true,
		  "total": 100,
		  "updated": 1,
		  "created": 2,
		  "deleted": 3,
		  "batches": 4,
		  "version_conflicts": 5,
		  "noops": 6,
		  "retries": {
			"bulk": 7,
			"search": 8
		  },
		  "failures": ["fail"]
		}
	  }
`
	res, err := decodeGetUpdateByQueryTaskResp([]byte(jsonData))
	require.NoError(t, err)
	require.NotNil(t, res.Response)

	require.True(t, res.Completed)
	require.True(t, res.Response.TimedOut)
	require.Equal(t, 100, res.Response.Total)
	require.Equal(t, 1, res.Response.Updated)
	require.Equal(t, 2, res.Response.Created)
	require.Equal(t, 3, res.Response.Deleted)
	require.Equal(t, 4, res.Response.Batches)
	require.Equal(t, 5, res.Response.VersionConflicts)
	require.Equal(t, 6, res.Response.Noops)
	require.Equal(t, 7, res.Response.Retries.Bulk)
	require.Equal(t, 8, res.Response.Retries.Search)
	require.Contains(t, res.Response.Failures, "fail")

	require.Equal(t, 100, res.Task.Status.Total)
	require.Equal(t, 1, res.Task.Status.Updated)
	require.Equal(t, 2, res.Task.Status.Created)
	require.Equal(t, 3, res.Task.Status.Deleted)
	require.Equal(t, 4, res.Task.Status.Batches)
	require.Equal(t, 5, res.Task.Status.VersionConflicts)
	require.Equal(t, 6, res.Task.Status.Noops)
	require.Equal(t, 7, res.Task.Status.Retries.Bulk)
	require.Equal(t, 8, res.Task.Status.Retries.Search)

	require.Equal(t, "6kynW36yQJG6Kd2qFpJ6YA", res.Task.Node)
	require.Equal(t, "transport", res.Task.Type)
	require.Equal(t, "indices:data/write/update/byquery", res.Task.Action)
	require.Equal(t, int64(170364), res.Task.Id)
	require.Equal(t, int64(1583092186694), res.Task.StartTimeInMillis)
	require.Equal(t, int64(32874403), res.Task.RunningTimeInNanos)
	require.Equal(t, map[string]string{"X-Opaque-Id": "123456"}, res.Task.Headers)

}
