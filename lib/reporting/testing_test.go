package reporting_test

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/reporting"
	"github.com/jedib0t/go-pretty/table"
)

func TestMiddleware(t *testing.T) {

	reportChan := make(chan reporting.VerfictionReport)
	var nodeInfoMap = make(map[string][]reporting.Info)
	wg := &sync.WaitGroup{}

	wg.Add(2)
	wr := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	tb := make(map[string]*reporting.Table)
	tb["AutomateStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["ChefServerStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["PostgresStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["OpenSearchStatusTable"] = &reporting.Table{
		Header:    table.Row{"No.", "Identifier", "Paramter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
	}
	tb["AutomateSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	tb["ChefServerSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	tb["PostgresSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}
	tb["OpenSearchSummaryTable"] = &reporting.Table{
		Header:    table.Row{"Paramter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 4, WidthMax: 50}},
	}

	report := reporting.NewReportingModule(wr, time.Second, tb)

	go chefserver(reportChan, wg)
	go automate(reportChan, wg)

	go reporting.VerfictionReports(reportChan, report, nodeInfoMap)
	wg.Wait()

	//close(reportChan)
	reporting.PrintStatusTable(report, nodeInfoMap)
}

func automate(reportChan chan reporting.VerfictionReport, wg *sync.WaitGroup) {
	defer wg.Done()

	// Read JSON file
	data, err := ioutil.ReadFile("data.json")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	var dataStruct []reporting.Info

	err = json.Unmarshal(data, &dataStruct)
	if err != nil {
		fmt.Println("Error unmarshaling JSON:", err)
		return
	}
	for _, value := range dataStruct {
		myReport := reporting.VerfictionReport{
			TableKey:     "Automate",
			Report:       value,
			TotalReports: 4,
		}
		reportChan <- myReport
	}
}

func chefserver(reportChan chan reporting.VerfictionReport, wg *sync.WaitGroup) {
	defer wg.Done()
	// Read JSON file
	data, err := ioutil.ReadFile("data.json")
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	var dataStruct []reporting.Info

	err = json.Unmarshal(data, &dataStruct)
	if err != nil {
		fmt.Println("Error unmarshaling JSON:", err)
		return
	}
	for _, value := range dataStruct {
		myReport := reporting.VerfictionReport{
			TableKey:     "ChefServer",
			Report:       value,
			TotalReports: 4,
		}
		reportChan <- myReport
	}

}

// package main

// import (
// 	"encoding/json"
// 	"fmt"
// 	"io/ioutil"
// 	"os"
// 	"time"

// 	"github.com/chef/automate/components/automate-deployment/pkg/cli"
// 	reporting "github.com/chef/automate/lib/reporting"
// 	"github.com/jedib0t/go-pretty/table"
// )

// type ServiceInfo struct {
// 	Hostip        string         `json:"hostip,omitempty"`
// 	Parameter     string         `json:"parameter,omitempty"`
// 	Status        string         `json:"status,omitempty"`
// 	StatusMessage *StatusMessage `json:"statusMessage,omitempty"`
// 	Error         error          `json:"error,omitempty"`
// }

// type StatusMessage struct {
// 	MainMessage string   `json:"mainMessage,omitempty"`
// 	SubMessage  []string `json:"subMessage,omitempty"`
// 	ToResolve   []string `json:"toResolve,omitempty"`
// }

// func main() {
// 	// Read JSON file
// 	data, err := ioutil.ReadFile("data.json")
// 	if err != nil {
// 		fmt.Println("Error reading file:", err)
// 		return
// 	}

// 	var dataStruct []ServiceInfo

// 	err = json.Unmarshal(data, &dataStruct)
// 	if err != nil {
// 		fmt.Println("Error unmarshaling JSON:", err)
// 		return
// 	}
// 	writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)

// 	tableConfig := &reporting.Table{
// 		Header:    table.Row{"No.", "Identifier", "Parameter", "Status", "Message"},
// 		ColConfig: []table.ColumnConfig{{Number: 5, WidthMax: 50}},
// 	}
// 	tableMap := map[string]*reporting.Table{"Automate": tableConfig}
// 	report := reporting.NewReportingModule(writer, time.Second, tableMap)

// 	for index, value := range dataStruct {
// 		appendRows(index+1, &value, report)
// 	}

// 	t := report.GenerateTableOutput(report.GetTable())
// 	fmt.Println(t)

// }

// func appendRows(index int, newEntry *ServiceInfo, report reporting.Reporting) {
// 	tb := report.GetTable()
// 	var row table.Row
// 	var rows []table.Row
// 	if newEntry.Status == "Success" {
// 		hostip := newEntry.Hostip
// 		parameter := newEntry.Parameter
// 		status := report.ChangeColour("green", report.AppendSpecialCharater(2, newEntry.Status))
// 		message := newEntry.StatusMessage.MainMessage

// 		row = table.Row{index, hostip, parameter, status, message}
// 		rows = append(rows, row)
// 	} else if newEntry.Status == "Failed" {
// 		hostip := report.ChangeColour("red", newEntry.Hostip)
// 		parameter := report.ChangeColour("red", newEntry.Parameter)
// 		status := report.ChangeColour("red", report.AppendSpecialCharater(1, newEntry.Status))
// 		message := report.ChangeColour("red", newEntry.StatusMessage.MainMessage)

// 		row = table.Row{index, hostip, parameter, status, message}
// 		rows = append(rows, row)
// 	}
// 	if len(newEntry.StatusMessage.SubMessage) > 0 {
// 		for _, value := range newEntry.StatusMessage.SubMessage {
// 			var newSubMessage string
// 			if newEntry.Status == "Failed" {
// 				key := report.AppendSpecialCharater(1, report.AppendSpecialCharater(4, newEntry.Status))
// 				newSubMessage = report.ChangeColour("red", key+value)
// 			} else {
// 				key := report.ChangeColour("yellow", report.AppendSpecialCharater(3, report.AppendSpecialCharater(4, "Waring")))
// 				newSubMessage = key + value
// 			}
// 			row = table.Row{"", "", "", "", newSubMessage}
// 			rows = append(rows, row)
// 		}
// 	}

// 	rows = append(tb.Rows, rows...)
// 	table := &reporting.Table{
// 		Header:    tb.Header,
// 		Rows:      rows,
// 		ColConfig: tb.ColConfig,
// 	}
// 	report.SetTables(table)
// }
