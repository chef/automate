package checkutils

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/stretchr/testify/assert"
)

func TestIsPassed(t *testing.T) {
	checks1 := []models.Checks{
		{Passed: true},
		{Passed: true},
		{Passed: true},
	}
	assert.True(t, IsPassed(checks1))

	checks2 := []models.Checks{
		{Passed: true},
		{Passed: false},
		{Passed: true},
	}
	assert.False(t, IsPassed(checks2))

	checks3 := []models.Checks{}
	assert.True(t, IsPassed(checks3))
}

func TestPrepareTriggerResponse_WithNoError(t *testing.T) {
	// Arrange
	resp := &models.CheckTriggerResponse{
		Status: "ok",
		Result: models.ApiResult{
			Passed:  true,
			Check:   "test check",
			Message: "test message",
			Checks:  []models.Checks{},
		},
	}
	host := "test host"
	nodeType := constants.AUTOMATE
	errorString := ""
	check := "test check"
	msg := "test message"
	isError := false
	statusCode := 200

	// Act
	result := PrepareTriggerResponse(resp, host, nodeType, errorString, check, msg, isError, statusCode)

	// Assert
	assert.Equal(t, resp.Status, result.Status)
	assert.Equal(t, host, result.Host)
	assert.Equal(t, nodeType, result.NodeType)
	assert.Equal(t, resp.Result.Passed, result.Result.Passed)
	assert.Equal(t, check, result.Result.Check)
	assert.Equal(t, msg, result.Result.Message)
	assert.Equal(t, len(resp.Result.Checks), len(result.Result.Checks))

}

func TestPrepareTriggerResponse_WithError(t *testing.T) {
	// Arrange
	resp := &models.CheckTriggerResponse{}
	host := "test host"
	nodeType := constants.AUTOMATE
	errorString := "test error"
	check := "test check"
	msg := "test message"
	isError := true
	statusCode := 400
	// Act
	result := PrepareTriggerResponse(resp, host, nodeType, errorString, check, msg, isError, statusCode)

	// Assert
	assert.Equal(t, resp.Status, result.Status)
	assert.Equal(t, host, result.Host)
	assert.Equal(t, nodeType, result.NodeType)
	assert.Equal(t, resp.Result.Passed, result.Result.Passed)
	assert.Equal(t, check, result.Result.Check)
	assert.Equal(t, msg, result.Result.Message)
	assert.Equal(t, len(resp.Result.Checks), len(result.Result.Checks))

}

func TestPrepareEndPoint(t *testing.T) {
	type args struct {
		ip   string
		port string
		path string
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{
			name: "Correct Enpoint",
			args: args{
				ip:   "127.0.0.1",
				port: "1234",
				path: "/test",
			},
			want: "http://127.0.0.1:1234/test",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := PrepareEndPoint(tt.args.ip, tt.args.port, tt.args.path)
			assert.Equal(t, got, tt.want)
		})
	}
}
