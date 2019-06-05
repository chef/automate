//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2019, Chef Software Inc.
//

package integration_test

import (
	"fmt"
	"math/rand"
	"testing"

	uuid "github.com/chef/automate/lib/uuid4"
)

const letterBytes = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

func RandString(n int) string {
	b := make([]byte, n)
	for i := range b {
		b[i] = letterBytes[rand.Intn(len(letterBytes))]
	}
	return string(b)
}

func BenchmarkIngestionNewServices(b *testing.B) {
	var criticality = []string{"ok", "critical", "warning", "unknown"}

	defer suite.DeleteDataFromStorage()

	for n := 0; n < b.N; n++ {
		b.StopTimer()
		var (
			uuid, _ = uuid.NewV4()
			svcName = RandString(10)
			health  = criticality[rand.Intn(4)]
		)

		event := NewHabitatEvent(
			withSupervisorId(uuid.String()),
			withPackageIdent(fmt.Sprintf("core/%s/0.1.0/20190101121212", svcName)),
			withServiceGroup(fmt.Sprintf("%s.default", svcName)),
			withStrategyAtOnce("stable"),
			withApplication("cool-app"),
			withEnvironment("development"),
			withFqdn(fmt.Sprintf("%s.example.com", svcName)),
			withHealth(health),
			withSite("us"),
		)

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
