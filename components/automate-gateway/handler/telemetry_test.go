package handler_test

import (
	"testing"

	"github.com/chef/automate/components/automate-gateway/handler"

	"github.com/stretchr/testify/assert"
)

func TestToUUIDFromUUID(t *testing.T) {
	deploymentId := "0d05b953-43b9-4990-9b11-3c0fd07ce8e2"
	result := handler.ToUUID(deploymentId)
	assert.Equal(t, deploymentId, result)
}
func TestToUUIDFrom16Bit(t *testing.T) {
	v5uuid := "f37ae1c0-da80-541a-a01a-31d34cb3ad19"
	deploymentId := "6cf8e74cf4603a3e"
	result := handler.ToUUID(deploymentId)
	assert.Equal(t, v5uuid, result)
}

func TestToUUIDFromEmpty(t *testing.T) {
	deploymentId := ""
	result := handler.ToUUID(deploymentId)
	assert.Equal(t, deploymentId, result)
}
