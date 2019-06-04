//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/api/external/habitat"
)

var (
	sgRes *applications.ServiceGroups
	sgErr error
)

func BenchmarkGetServiceGroups(b *testing.B) {
	defer suite.DeleteDataFromStorage()

	var (
		ctx            = context.Background()
		request        = new(applications.ServiceGroupsReq)
		iterations     = 20
		incremental    = 50
		maxGroups      = incremental * iterations
		nServiceGroups = 0
	)

	for i := 0; i <= iterations; i++ {
		suite.IngestServices(habNServiceGroups(nServiceGroups))

		b.Run(fmt.Sprintf("max_%d_with_%d_groups", maxGroups, nServiceGroups), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				sgRes, sgErr = suite.ApplicationsServer.GetServiceGroups(ctx, request)
			}
		})

		nServiceGroups = nServiceGroups + incremental
	}
}

func BenchmarkGetServiceGroupsHeavyLoad(b *testing.B) {
	defer suite.DeleteDataFromStorage()

	var (
		ctx            = context.Background()
		request        = new(applications.ServiceGroupsReq)
		iterations     = 5
		incremental    = 1000
		maxGroups      = incremental * iterations
		nServiceGroups = 0
	)

	for i := 0; i <= iterations; i++ {
		suite.IngestServices(habNServiceGroups(nServiceGroups))

		b.Run(fmt.Sprintf("max_%d_with_%d_groups", maxGroups, nServiceGroups), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				sgRes, sgErr = suite.ApplicationsServer.GetServiceGroups(ctx, request)
			}
		})

		nServiceGroups = nServiceGroups + incremental
	}
}

func habNServiceGroups(total int) []*habitat.HealthCheckEvent {
	eventArray := make([]*habitat.HealthCheckEvent, total)

	for n := 0; n < total; n++ {
		eventArray[n] = NewHabitatEvent(
			withSupervisorId(fmt.Sprintf("s-%d", n)),
			withServiceGroup(fmt.Sprintf("app-%d.default", n)),
			withPackageIdent(fmt.Sprintf("core/app-%d/0.1.0/20190101121212", n)),
		)
	}

	return eventArray
}
