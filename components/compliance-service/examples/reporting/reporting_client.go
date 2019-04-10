package main

import (
	"flag"
	"log"

	rs "github.com/chef/automate/components/compliance-service/api/reporting"
	reportingtest "github.com/chef/automate/components/compliance-service/examples/reporting/test"
)

const (
	address = "localhost:2134"
)

func main() {
	exportPtr := flag.String("export-type", "json", "Type to export profile to (json or csv)")
	flag.Parse()

	res, err := reportingtest.Export(&rs.Query{
		Type: *exportPtr,
	})
	if err != nil {
		log.Printf("error, %v", err)
	}
	log.Printf("Profile Export, %v", res)
}
