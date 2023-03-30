package verification

import (
	"fmt"
	"os"
	"sync"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/reporting"
	"github.com/jedib0t/go-pretty/table"
)

type verifySetup interface {
	VerifyHAAWSProvision(string)
	VerifyHAAWSManagedProvision(string)
	VerifyHAAWSDeployment(string)
	VerifyHAAWSManagedDeployment(string)
	VerifyOnPremDeployment(string)
	VerifyOnPremAWSManagedDeployment(string)
	VerifyOnPremCustManagedDeployment(string)
	VerifyStandaloneDeployment(string)
	VerifyCertificates(string)
}

type report struct {
	ipaddress             string
	parameter             string
	message               string
	errors                []string
	resolutions           []string
	successfulChecksCount int
	failedChecksCount     int
}

type numberOfNodes struct {
	numberOfAutomateNodes    int
	numberOfChefServerNodes  int
	numberOfPostgresSQLNodes int
	numberOfOpenSearchNodes  int
}

var reportChan = make(chan reporting.VerfictionReport)
var nodeInfoMap = make(map[string][]reporting.Info)
var doneChan = make(chan bool, 1)

func VerifyHAAWSProvision(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}

	validateAWSProvisionConfig(config)
	return nil
}

func VerifyHAAWSManagedProvision(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSManagedProvisionConfig(config)
	return nil
}

func VerifyHAAWSDeployment(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSDeploymentConfig(config)
	return nil
}

func VerifyHAAWSManagedDeployment(configFile string) error {
	config, err := parseAWSAutomateConfig(configFile)
	if err != nil {
		return err
	}
	validateAWSManagedDeploymentConfig(config)
	return nil
}

func VerifyOnPremAWSManagedDeployment(configFile string) error {
	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremAWSConfig(config)
	return nil
}

func VerifyOnPremCustManagedDeployment(configFile string) error {
	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	validateOnPremCustomerConfig(config)
	return nil
}

func VerifyStandaloneDeployment(configFile string) error {
	config, err := parseStandaloneConfig(configFile)
	if err != nil {
		return err
	}
	validateStandaloneDeploymentConfig(config)
	return nil
}

func VerifyCertificates(certContents string) error {

	return nil
}

func VerifyOnPremDeployment(configFile string) error {

	config, err := parseOnPremConfig(configFile)
	if err != nil {
		return err
	}
	var ipsMap = make(map[string]string)
	automateIps := config.ExistingInfra.Config.AutomatePrivateIps
	chefServerIps := config.ExistingInfra.Config.ChefServerPrivateIps
	opensearchIps := config.ExistingInfra.Config.OpensearchPrivateIps
	postgresIps := config.ExistingInfra.Config.PostgresqlPrivateIps

	dataMap := map[string][]string{
		"Automate":    automateIps,
		"ChefServer":  chefServerIps,
		"OpenSearch":  opensearchIps,
		"PostgresSQL": postgresIps,
	}

	for nodeType, nodeIps := range dataMap {
		constructMap(nodeIps, nodeType, &ipsMap)
	}

	numberOfNodes := &numberOfNodes{
		numberOfAutomateNodes:    len(automateIps),
		numberOfChefServerNodes:  len(chefServerIps),
		numberOfPostgresSQLNodes: len(postgresIps),
		numberOfOpenSearchNodes:  len(opensearchIps),
	}

	startReportModule(numberOfNodes)

	//validateOnPremConfig(config)

	runTests(ipsMap)

	return nil
}

func constructMap(nodeIps []string, nodeType string, m *map[string]string) {
	for _, val := range nodeIps {
		(*m)[val] = nodeType
	}
}

func runTests(ipsMap map[string]string) {
	var wg sync.WaitGroup

	for k, v := range ipsMap {
		ip := fmt.Sprintf("%v", k)
		nodeType := fmt.Sprintf("%v", v)
		if nodeType == "OpenSearch" || nodeType == "PostgresSQL" {
			wg.Add(4)
			go func() {
				defer wg.Done()
				producerChan := make(chan bool, 1)
				runchecksForCertificates(ip, nodeType, producerChan)
			}()
		} else {
			wg.Add(3)
		}
		go func() {
			defer wg.Done()
			validateNodeReachability(ip, nodeType, reportChan)
		}()
		go func() {
			defer wg.Done()
			runchecksForSystemResources(ip, nodeType)
		}()
		go func() {
			defer wg.Done()
			runchecksForSoftwareVersions(ip, nodeType)
		}()
	}
	wg.Wait()
	close(reportChan)
	if <-doneChan {
		fmt.Println("done!!")
	}
}

func runchecksForCertificates(ipaddress string, nodeType string, doneChan chan bool) {
	var successCount, failedCount int
	result := validateCertificateFormat(ipaddress, "abc")
	if !result.Valid {
		failedCount++
		result.Report.SummaryInfo.FailedCount = failedCount
		chanWriter(reportChan, nodeType, result.Report)
	} else {
		result = validateCertificateExpiry(ipaddress, "abc")
		if !result.Valid {
			failedCount++
			result.Report.SummaryInfo.FailedCount = failedCount
			chanWriter(reportChan, nodeType, result.Report)
		} else {
			successCount++
			report := report{
				ipaddress:             ipaddress,
				parameter:             "Certificates",
				message:               "Cerificate validation successful",
				successfulChecksCount: successCount,
			}
			chanWriter(reportChan, nodeType, getReport(report, "Success"))
		}
	}
	doneChan <- true
}

func runchecksForSystemResources(ipaddress string, nodeType string) {
	var successCount, failedCount int
	var errors []string
	var resolutions []string
	failed := false

	result := validateFreeDisk(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		failed = true
		failedCount++
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		successCount++
	} else if result.Valid {
		successCount++
	}

	result = validateCPU(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		failed = true
		failedCount++
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		successCount++
	} else if result.Valid {
		successCount++
	}

	if failed {
		report := report{
			ipaddress:             ipaddress,
			parameter:             "System Resources",
			message:               "System Resource Check failed",
			errors:                errors,
			resolutions:           resolutions,
			failedChecksCount:     failedCount,
			successfulChecksCount: successCount,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Failed"))

	} else {
		report := report{
			ipaddress:             ipaddress,
			parameter:             "System Resources",
			message:               "All System Resources are as per the requirement",
			errors:                errors,
			resolutions:           resolutions,
			failedChecksCount:     failedCount,
			successfulChecksCount: successCount,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Success"))
	}
}

func runchecksForSoftwareVersions(ipaddress string, nodeType string) {
	var successCount, failedCount int
	var errors []string
	var resolutions []string
	failed := false

	result := validateOSVersion(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		failed = true
		failedCount++
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		successCount++
	} else if result.Valid {
		successCount++
	}

	result = validateLinuxCommands(ipaddress, nodeType)
	if !result.Valid {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		failed = true
		failedCount++
	} else if result.Valid && result.Report.StatusMessage.SubMessage != nil {
		errors = append(errors, result.Report.StatusMessage.SubMessage...)
		resolutions = append(resolutions, result.Report.SummaryInfo.ToResolve...)
		successCount++
	} else if result.Valid {
		successCount++
	}

	if failed {
		report := report{
			ipaddress:             ipaddress,
			parameter:             result.Report.Parameter,
			message:               result.Report.StatusMessage.MainMessage,
			errors:                errors,
			resolutions:           resolutions,
			failedChecksCount:     failedCount,
			successfulChecksCount: successCount,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Failed"))

	} else {
		report := report{
			ipaddress:             ipaddress,
			parameter:             result.Report.Parameter,
			message:               result.Report.StatusMessage.MainMessage,
			errors:                errors,
			resolutions:           resolutions,
			failedChecksCount:     failedCount,
			successfulChecksCount: successCount,
		}
		chanWriter(reportChan, nodeType, getReport(report, "Success"))
	}
}

func getReport(testReport report, status string) reporting.Info {
	return reporting.Info{
		Hostip:    testReport.ipaddress,
		Parameter: testReport.parameter,
		Status:    status,
		StatusMessage: &reporting.StatusMessage{
			MainMessage: testReport.message,
			SubMessage:  testReport.errors,
		},
		SummaryInfo: &reporting.SummaryInfo{
			ToResolve:       testReport.resolutions,
			SuccessfulCount: testReport.successfulChecksCount,
			FailedCount:     testReport.failedChecksCount,
		},
	}
}

func startReportModule(nodeNumbers *numberOfNodes) {
	wr := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	tb := createTables(nodeNumbers)
	rp := reporting.NewReportingModule(wr, tb)
	go reporting.VerificationReports(reportChan, rp, nodeInfoMap, doneChan)
}

func createTables(nodeNumbers *numberOfNodes) map[string]*reporting.Table {
	automateSummaryTableTitle := fmt.Sprintf("Summary: Automate Nodes- %d", nodeNumbers.numberOfAutomateNodes)
	chefServerSummaryTableTitle := fmt.Sprintf("Summary: Chef Infra Server Nodes- %d", nodeNumbers.numberOfChefServerNodes)
	postgresSQLSummaryTableTitle := fmt.Sprintf("Summary: PostgresSQL Nodes- %d", nodeNumbers.numberOfPostgresSQLNodes)
	openSearchSummaryTableTitle := fmt.Sprintf("Summary: Opensearch Nodes- %d", nodeNumbers.numberOfOpenSearchNodes)
	tb := make(map[string]*reporting.Table)
	tb["AutomateStatusTable"] = getStatusTable("Automate")
	tb["AutomateSummaryTable"] = getSummaryTable(automateSummaryTableTitle)
	tb["ChefServerStatusTable"] = getStatusTable("Chef Infra Server")
	tb["ChefServerSummaryTable"] = getSummaryTable(chefServerSummaryTableTitle)
	tb["PostgresSQLStatusTable"] = getStatusTable("PostgresSQL")
	tb["PostgresSQLSummaryTable"] = getSummaryTable(postgresSQLSummaryTableTitle)
	tb["OpenSearchStatusTable"] = getStatusTable("OpenSearch")
	tb["OpenSearchSummaryTable"] = getSummaryTable(openSearchSummaryTableTitle)
	return tb
}

func getStatusTable(title string) *reporting.Table {
	return &reporting.Table{
		Title:     title,
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 1, WidthMax: 5, WidthMin: 5}, {Number: 2, WidthMax: 15, WidthMin: 15}, {Number: 3, WidthMax: 25, WidthMin: 25}, {Number: 4, WidthMax: 15, WidthMin: 15}, {Number: 5, WidthMax: 60, WidthMin: 60}},
	}
}

func getSummaryTable(title string) *reporting.Table {
	return &reporting.Table{
		Title:     title,
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 1, WidthMax: 30, WidthMin: 30}, {Number: 2, WidthMax: 15, WidthMin: 15}, {Number: 3, WidthMax: 15, WidthMin: 15}, {Number: 4, WidthMax: 60, WidthMin: 60}},
	}
}

func chanWriter(reportChan chan reporting.VerfictionReport, nodeType string, report reporting.Info) {
	total := 3
	if nodeType == "OpenSearch" || nodeType == "PostgresSQL" {
		total = 12
	}
	msg := reporting.VerfictionReport{
		TableKey:     nodeType,
		Report:       report,
		TotalReports: total,
	}
	reportChan <- msg
}
