package integration_test

import (
	"fmt"
	"os"
	"testing"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/chef/automate/components/compliance-service/ingest/pipeline/processor"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/olivere/elastic/v7"
)

type Node struct {
	ReportUUID string `json:"uuid"`
}

func TestParseReportCtrlStruct(t *testing.T) {
	esclient, err := elastic.NewClient(
		elastic.SetURL(opensearchUrl),
		elastic.SetSniff(false),
	)
	if err != nil {
		fmt.Printf("Could not create elasticsearch client from %q: %s\n", opensearchUrl, err)
		os.Exit(1)
	}
	ingesticESClient := ingestic.NewESClient(esclient)
	inspecReport := relaxting.ESInSpecReport{ReportID: "19c92848-8115-470e-a19f-59869ef9ad31"}
	processor.ParseReportCtrlStruct(ingesticESClient, &inspecReport)
}
