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

	// Act
	result := PrepareTriggerResponse(resp, host, nodeType, errorString, check, msg, isError)

	// Assert
	if result.Status != resp.Status {
		t.Errorf("Expected status %s, but got %s", resp.Status, result.Status)
	}
	if result.Host != host {
		t.Errorf("Expected host %s, but got %s", host, result.Host)
	}
	if result.NodeType != nodeType {
		t.Errorf("Expected node type %s, but got %s", nodeType, result.NodeType)
	}
	if result.Result.Passed != resp.Result.Passed {
		t.Errorf("Expected passed %t, but got %t", resp.Result.Passed, result.Result.Passed)
	}
	if result.Result.Check != check {
		t.Errorf("Expected check %s, but got %s", check, result.Result.Check)
	}
	if result.Result.Message != msg {
		t.Errorf("Expected message %s, but got %s", msg, result.Result.Message)
	}
	if len(result.Result.Checks) != len(resp.Result.Checks) {
		t.Errorf("Expected checks length %d, but got %d", len(resp.Result.Checks), len(result.Result.Checks))
	}
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

	// Act
	result := PrepareTriggerResponse(resp, host, nodeType, errorString, check, msg, isError)

	// Assert
	if result.Status != "" {
		t.Errorf("Expected empty status, but got %s", result.Status)
	}
	if result.Host != host {
		t.Errorf("Expected host %s, but got %s", host, result.Host)
	}
	if result.NodeType != nodeType {
		t.Errorf("Expected node type %s, but got %s", nodeType, result.NodeType)
	}
	if result.Result.Passed != false {
		t.Errorf("Expected passed false, but got %t", result.Result.Passed)
	}
	if result.Result.Check != check {
		t.Errorf("Expected check %s, but got %s", check, result.Result.Check)
	}
	if result.Result.Message != msg {
		t.Errorf("Expected message %s, but got %s", msg, result.Result.Message)
	}
}
