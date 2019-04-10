package main

import (
	"context"
	"time"

	"github.com/sirupsen/logrus"

	"fmt"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/components/compliance-service/ingest/ingestic/mappings"
	"github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

func main() {
	manualTriggerTest()
}

func manualTriggerTest() {
	// The config file to use
	/*
		[ingest.v1.sys.service]
		purge_actions_after_days = 2
		purge_converge_history_after_days = 2

		[compliance.v1.sys.retention]
		compliance_report_days = 2
	*/

	tlsConfig := certs.TLSConfig{
		CertPath:       "/hab/svc/data-lifecycle-service/config/service.crt",
		KeyPath:        "/hab/svc/data-lifecycle-service/config/service.key",
		RootCACertPath: "/hab/svc/data-lifecycle-service/config/root_ca.crt",
	}
	serviceCerts, err := tlsConfig.ReadCerts()
	if err != nil {
		panic(err)
	}
	connFactory := secureconn.NewFactory(*serviceCerts)
	conn, err := connFactory.Dial("data-lifecycle-service", "localhost:10129")
	if err != nil {
		panic(err)
	}

	es, err := elastic.New("http://localhost:10141")
	if err != nil {
		panic(err)
	}

	es.CreateTimeNamedIndices(context.Background(), time.Now(), "converge-history", 5)
	es.CreateTimeNamedIndices(context.Background(), time.Now(), "actions", 5)
	name := fmt.Sprintf("comp-%s-s", mappings.ComplianceCurrentTimeSeriesIndicesVersion)
	es.CreateTimeNamedIndices(context.Background(), time.Now(), name, 5)
	name = fmt.Sprintf("comp-%s-r", mappings.ComplianceCurrentTimeSeriesIndicesVersion)
	es.CreateTimeNamedIndices(context.Background(), time.Now(), name, 5)

	if err != nil {
		panic(err)
	}

	client := data_lifecycle.NewDataLifecycleClient(conn)

	dlsResp, err := client.TriggerPurge(context.Background(), &data_lifecycle.TriggerPurgeRequest{
		Id: "my-id",
	})

	if err != nil {
		logrus.WithError(err).Fatal("Failed")
	}

	for service, resp := range dlsResp.Responses {
		for component, status := range resp.ComponentStatus {
			if status.Status != data_lifecycle.PurgeStatus_SUCCESS {
				logrus.WithFields(logrus.Fields{
					"service":   service,
					"component": component,
					"msg":       status.Msg,
				}).Fatal("Component failed")
			}
		}
	}

	//expectNumIndicesFor(es, "actions", 2)
	//expectNumIndicesFor(es, "converge-history", 2)
	expectNumIndicesFor(es, fmt.Sprintf("comp-%s-s", mappings.ComplianceCurrentTimeSeriesIndicesVersion), 2)
	expectNumIndicesFor(es, fmt.Sprintf("comp-%s-r", mappings.ComplianceCurrentTimeSeriesIndicesVersion), 2)
}

func expectNumIndicesFor(es *elastic.Elastic, baseName string, num int) {
	maxTries := 3
	for tries := 1; tries <= maxTries; tries++ {
		indices, err := es.DailyIndicesFor(context.Background(), baseName)
		if err != nil {
			panic(err)
		}

		if len(indices) != num {
			if tries == maxTries {
				logrus.WithFields(logrus.Fields{
					"baseName": baseName,
					"expected": num,
					"found":    len(indices),
					"tries":    tries,
				}).Fatal("Unexpected number of index found")
			} else {
				time.Sleep(1 * time.Second)
			}
		} else {
			break
		}
	}

}
