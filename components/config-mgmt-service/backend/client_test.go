//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package backend_test

// The recommended pattern to write tests in Go is to create a different
// module so you can test the externals of your Go package, although if
// we DO need to test the internals of our package you should totally
// extend the package in a different file. (Ex. `backend_internal_test.go`)

import (
	"testing"

	b "github.com/chef/automate/components/config-mgmt-service/backend"
)

var totalNodesTestsMatrix = []struct {
	subject  b.NodesCounts
	expected int64
}{
	{b.NodesCounts{5, 10, 0, 0}, 15},
	{b.NodesCounts{1234, 2432, 3123, 0}, 6789},
	{b.NodesCounts{0, 0, 0, 0}, 0},
	{b.NodesCounts{10, 10, 10, 0}, 30},
	{b.NodesCounts{59999, 1, 1, 0}, 60001},
	{b.NodesCounts{111, 222, 555, 0}, 888},
	{b.NodesCounts{1, 1, 1, 0}, 3},
}

func TestComputeTotalNodes(t *testing.T) {
	for _, matrix := range totalNodesTestsMatrix {
		s := matrix.subject
		e := matrix.expected
		if s.ComputeTotalNodes(); s.Total != e {
			t.Errorf("Expected %d total nodes, but it was %d instead.", e, s.Total)
		}
	}
}
