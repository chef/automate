package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestHaNodeDeleteFactoryIfDeployerTypeMatchesFlagAWS(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		awsMode: true,
	}
	deployerType := AWS_MODE
	_, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.NoError(t, err)

}

func TestHaNodeDeleteFactoryIfDeployerTypeMatchesFlagOnPrem(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		onPremMode: true,
	}
	deployerType := EXISTING_INFRA_MODE
	deleteNode, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.NoError(t, err)
	_, ok := deleteNode.(*DeleteNodeOnPremImpl)
	assert.True(t, ok)
}

func TestHaNodeDeleteFactoryIfNoFlagGivenTypeOnprem(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{}
	deployerType := EXISTING_INFRA_MODE
	deleteNode, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.NoError(t, err)
	_, ok := deleteNode.(*DeleteNodeOnPremImpl)
	assert.True(t, ok)
}

func TestHaNodeDeleteFactoryIfNoFlagGivenTypeAWS(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{}
	deployerType := AWS_MODE
	deleteNode, err := haAddNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.NoError(t, err)
	_, ok := deleteNode.(*AddNodeAWSImpl)
	assert.True(t, ok)
}

func TestHaNodeDeleteFactoryIfDeployerTypeDoesNotMatchFlagAWS(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		onPremMode: true,
	}
	deployerType := AWS_MODE
	_, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "flag given does not match with the current deployment type AWS_MODE. Try with --aws-mode flag")
}

func TestHaNodeDeleteFactoryIfDeployerTypeDoesNotMatchFlagOnPrem(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		awsMode: true,
	}
	deployerType := EXISTING_INFRA_MODE
	_, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "flag given does not match with the current deployment type EXISTING_INFRA_MODE. Try with --onprem-mode flag")
}

func TestHaNodeDeleteFactoryIfBothflagGiven(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		awsMode:    true,
		onPremMode: true,
	}
	deployerType := EXISTING_INFRA_MODE
	_, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "Cannot use both --onprem-mode and --aws-mode together. Provide only one at a time")
}

func TestHaNodeDeleteFactoryWithFlagOrDeployerModeNotMatch(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{
		awsMode: true,
	}
	deployerType := "SOMETHING_ELSE"
	_, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "unsupported deployment type. Current deployment type is SOMETHING_ELSE")
}

func TestHaNodeDeleteFactoryNoFlagOrDeployerModeNotMatch(t *testing.T) {
	addDeleteNodeHACmdFlags := &AddDeleteNodeHACmdFlags{}
	deployerType := "SOMETHING_ELSE"
	_, err := haDeleteNodeFactory(addDeleteNodeHACmdFlags, deployerType, getMockStatusSummary())
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "unsupported deployment type. Current deployment type is SOMETHING_ELSE")
}
