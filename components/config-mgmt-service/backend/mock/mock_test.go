//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package mock_test

import (
	"testing"
	"time"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	subject "github.com/chef/automate/components/config-mgmt-service/backend/mock"
	"github.com/stretchr/testify/assert"
)

// Maybe it would be a good idea to import the package we
// are trying to test renaming it to be `subject` so we know
// the subject we are actually testing

func TestNew(t *testing.T) {
	// We can't test the "dummy" client because
	// it is a private attribute
	subject.New()
}

func TestNodeExists(t *testing.T) {
	expected := "999"
	exists, err := subject.New().NodeExists(expected, map[string][]string{})
	assert.Nil(t, err)
	assert.False(t, exists)
}

func TestGetNodesCounts(t *testing.T) {
	expected := *new(backend.NodesCounts)
	filterMap := make(map[string][]string)
	nodeState, err := subject.New().GetNodesCounts(filterMap)
	assert.Nil(t, err)
	assert.Equal(t, nodeState, expected)
}

func TestGetRunsCounts(t *testing.T) {
	expected := *new(backend.RunsCounts)
	filterMap := make(map[string][]string)
	runState, err := subject.New().GetRunsCounts(filterMap, "", "", "")
	assert.Nil(t, err)
	assert.Equal(t, runState, expected)
}

func TestGetAttribute(t *testing.T) {
	expected := *new(backend.NodeAttribute)
	attribute, err := subject.New().GetAttribute("")
	assert.Nil(t, err)
	assert.Equal(t, attribute, expected)
}

func TestGetActions(t *testing.T) {
	expected := make([]backend.Action, 0)
	expected = append(expected, backend.Action{})
	filterMap := make(map[string][]string)
	actions, total, err := subject.New().GetActions(filterMap, time.Time{}, time.Time{}, 0,
		time.Time{}, "", true)
	assert.Nil(t, err)
	assert.Equal(t, actions, expected)
	assert.Equal(t, total, int64(0))
}
