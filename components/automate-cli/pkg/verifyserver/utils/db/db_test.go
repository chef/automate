package db

import (
	"strings"
	"testing"
)

func TestInitPostgresDB(t *testing.T) {
	//create DBImpl
	di := &DBImpl{}

	err := di.InitPostgresDB("mock-con-sting")
	expectedError := `missing "=" after "mock-con-sting" in connection info string`
	actualError := strings.TrimRight(err.Error(), `"`)

	if actualError != expectedError {
		t.Errorf("Extected error '%s', got '%s'", expectedError, actualError)
	}
}
