package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestHaNodeAddFactoryIfDeployerTypeMatchesFlagAWS(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		awsMode: true,
	}
	deployerType := AWS_MODE
	addNode, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.NoError(t, err)
	_, ok := addNode.(*AddNodeAWSImpl)
	assert.True(t, ok)
}

func TestHaNodeAddFactoryIfDeployerTypeMatchesFlagOnPrem(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		onPremMode: true,
	}
	deployerType := EXISTING_INFRA_MODE
	addNode, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.NoError(t, err)
	_, ok := addNode.(*AddNodeOnPremImpl)
	assert.True(t, ok)
}

func TestHaNodeAddFactoryIfNoFlagGivenTypeOnprem(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{}
	deployerType := EXISTING_INFRA_MODE
	addNode, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.NoError(t, err)
	_, ok := addNode.(*AddNodeOnPremImpl)
	assert.True(t, ok)
}

func TestHaNodeAddFactoryIfNoFlagGivenTypeAWS(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{}
	deployerType := AWS_MODE
	addNode, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.NoError(t, err)
	_, ok := addNode.(*AddNodeAWSImpl)
	assert.True(t, ok)
}

func TestHaNodeAddFactoryIfDeployerTypeDoesNotMatchFlagAWS(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		onPremMode: true,
	}
	deployerType := AWS_MODE
	_, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Flag given does not match with the current deployment type AWS_MODE. Try with --aws-mode flag")
}

func TestHaNodeAddFactoryIfDeployerTypeDoesNotMatchFlagOnPrem(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		awsMode: true,
	}
	deployerType := EXISTING_INFRA_MODE
	_, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Flag given does not match with the current deployment type EXISTING_INFRA_MODE. Try with --onprem-mode flag")
}

func TestHaNodeAddFactoryIfBothflagGiven(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		awsMode:    true,
		onPremMode: true,
	}
	deployerType := EXISTING_INFRA_MODE
	_, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Cannot use both --onprem-mode and --aws-mode together. Provide only one at a time")
}

func TestHaNodeAddFactoryWithFlagOrDeployerModeNotMatch(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		awsMode: true,
	}
	deployerType := "SOMETHING_ELSE"
	_, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Unsupported deployment type. Current deployment type is SOMETHING_ELSE")
}

func TestHaNodeAddFactoryNoFlagOrDeployerModeNotMatch(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{}
	deployerType := "SOMETHING_ELSE"
	_, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Unsupported deployment type. Current deployment type is SOMETHING_ELSE")
}
