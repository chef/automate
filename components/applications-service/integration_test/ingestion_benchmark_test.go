//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"testing"
)

func BenchmarkIngestionNewServices(b *testing.B) {
	defer suite.DeleteDataFromStorage()

	for n := 0; n < b.N; n++ {
		b.StopTimer()
		event := NewHabitatEventRandomized()

		b.StartTimer()
		suite.IngestService(event)
	}
}

func BenchmarkIngestionUpdateServices(b *testing.B) {
	b.StopTimer()
	defer suite.DeleteDataFromStorage()
	event := NewHabitatEvent(
		withSupervisorId("1234"),
		withPackageIdent("core/app/0.1.0/20190101121212"),
		withServiceGroup("app.default"),
		withStrategyAtOnce("stable"),
		withApplication("cool-app"),
		withEnvironment("development"),
		withFqdn("app.example.com"),
		withHealth("CRITICAL"),
		withSite("us"),
	)
	suite.IngestService(event)

	b.StartTimer()
	for n := 0; n < b.N; n++ {
		suite.IngestService(event)
	}
}
