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
	sRes *applications.ServicesRes
	sErr error
)

func BenchmarkGetServices(b *testing.B) {
	defer suite.DeleteDataFromStorage()

	var (
		ctx         = context.Background()
		request     = new(applications.ServicesReq)
		iterations  = 20
		incremental = 50
		maxServices = incremental * iterations
		nServices   = 0
	)

	for i := 0; i <= iterations; i++ {
		suite.IngestServices(habNServices(nServices))

		b.Run(fmt.Sprintf("max_%d_with_%d_services", maxServices, nServices), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				sRes, sErr = suite.ApplicationsServer.GetServices(ctx, request)
			}
		})

		nServices = nServices + incremental
	}
}

func BenchmarkGetServicesHeavyLoad(b *testing.B) {
	defer suite.DeleteDataFromStorage()

	var (
		ctx         = context.Background()
		request     = new(applications.ServicesReq)
		iterations  = 5
		incremental = 1000
		maxServices = incremental * iterations
		nServices   = 0
	)

	for i := 0; i <= iterations; i++ {
		suite.IngestServices(habNServices(nServices))

		b.Run(fmt.Sprintf("max_%d_with_%d_services", maxServices, nServices), func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				sRes, sErr = suite.ApplicationsServer.GetServices(ctx, request)
			}
		})

		nServices = nServices + incremental
	}
}

func habNServices(total int) []*habitat.HealthCheckEvent {
	eventArray := make([]*habitat.HealthCheckEvent, total)

	for n := 0; n < total; n++ {
		eventArray[n] = NewHabitatEvent(
			withSupervisorId(fmt.Sprintf("s-%d", n)),
			withServiceGroup("app.default"),
			withPackageIdent("core/app/0.1.0/20190101121212"),
		)
	}

	return eventArray
}
