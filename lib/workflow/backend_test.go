package workflow

import (
	_ "github.com/lib/pq"
)

type TestStruct struct {
	Thing string
}

/*
func TestTasks(t *testing.T) {
	connInfo := "postgresql://postgres@127.0.0.1:5432/workflow?sslmode=disable"
	pg, err := NewPostgresBackend(connInfo)
	require.NoError(t, err)
	err = pg.EnqueueTask(context.Background(), &Task{
		Name:               "bob",
		WorkflowInstanceID: 1,
		Parameters: &TestStruct{
			Thing: "foo",
		},
	})
	require.NoError(t, err)
}
*/
