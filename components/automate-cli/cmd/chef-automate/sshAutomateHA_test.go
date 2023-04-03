package main

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/require"
)

func TestGetAutomateHAInfraDetails(t *testing.T) {
	expectedInfraDetails := &AutomateHAInfraDetails{} // Define your expected infrastructure details here

	tmpfile, err := ioutil.TempFile("", "test-output.json")
	require.NoError(t, err)

	defer os.Remove(tmpfile.Name())

	n, err := tmpfile.Write([]byte(`{}`))
	require.NoError(t, err)
	require.NotZero(t, n)

	err = tmpfile.Close()
	require.NoError(t, err)

	actualInfraDetails, err := getAutomateHAInfraDetails(tmpfile.Name())
	require.NoError(t, err)

	require.Equal(t, actualInfraDetails, expectedInfraDetails)
}
