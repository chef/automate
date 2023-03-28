package reporting

import (
	"fmt"
	"strconv"

	"github.com/jedib0t/go-pretty/table"
)

type VerfictionReport struct {
	TableKey     string
	Report       Info
	TotalReports int
}

var NodeInfoMap = make(map[string][]Info)

type Info struct {
	Hostip        string         `json:"hostip,omitempty"`
	Parameter     string         `json:"parameter,omitempty"`
	Status        string         `json:"status,omitempty"`
	StatusMessage *StatusMessage `json:"statusMessage,omitempty"`
}

type StatusMessage struct {
	MainMessage string   `json:"mainMessage,omitempty"`
	SubMessage  []string `json:"subMessage,omitempty"`
	ToResolve   []string `json:"toResolve,omitempty"`
}

var SummaryMap map[string]SummaryInfo

type SummaryInfo struct {
	SuccessfulCount int
	FailedCount     int
	ToResolve       []string
}

func VerfictionReports(reportChan chan VerfictionReport, reporting Reporting, nodeInfoMap map[string][]Info) {

	//var report VerfictionReport
	for n := range reportChan {
		report := n
		info := report.Report
		//total := report.TotalReports
		node := report.TableKey
		var nodeinfo []Info
		fmt.Println("Line 46:", info)
		if _, ok := nodeInfoMap[node]; ok {
			nodeinfo := nodeInfoMap[node]
			nodeinfo = append(nodeinfo, info)
			nodeInfoMap[node] = nodeinfo
			createTables(reporting, nodeInfoMap)
		} else {
			nodeinfo = append(nodeinfo, info)
			nodeInfoMap[node] = nodeinfo
			createTables(reporting, nodeInfoMap)
		}
		PrintStatusTable(reporting, nodeInfoMap)
	}

}

func PrintStatusTable(reporting Reporting, nodeInfo map[string][]Info) {
	for key, _ := range nodeInfo {
		fmt.Println(key)
		tb := reporting.GetTable(key + "StatusTable")
		tb1 := reporting.GetTable(key + "SummaryTable")
		fmt.Println(reporting.GenerateTableOutput(tb))
		fmt.Println(reporting.GenerateTableOutput(tb1))
	}
}
func createTables(reporting Reporting, nodeInfo map[string][]Info) {
	for key, value := range nodeInfo {
		statusTableRows, summaryTableRows := createSingleNodeTable(value, reporting)
		reporting.GetTable(key + "StatusTable").Rows = statusTableRows
		reporting.GetTable(key + "SummaryTable").Rows = summaryTableRows
	}
}
func createSingleNodeTable(nodeInfo []Info, reporting Reporting) ([]table.Row, []table.Row) {
	var summaryMap = make(map[string]SummaryInfo)
	var statusTableRows []table.Row
	var summaryTableRows []table.Row
	for idx, value := range nodeInfo {
		singleRowEntry := createStatusTableRows(idx+1, &value, reporting)
		statusTableRows = append(statusTableRows, singleRowEntry...)
		summaryMap = createSummaryTableData(summaryMap, &value)
	}
	summaryTableRows = createSummaryTableRows(summaryMap, reporting)
	return statusTableRows, summaryTableRows
}
func createStatusTableRows(index int, rowData *Info, reporting Reporting) []table.Row {
	var row table.Row
	var rows []table.Row

	if rowData.Status == "Success" {
		hostip := rowData.Hostip
		parameter := rowData.Parameter
		status := reporting.ChangeColour("green", reporting.AppendSpecialCharater(2, rowData.Status))
		message := rowData.StatusMessage.MainMessage
		row = table.Row{index, hostip, parameter, status, message}
		rows = append(rows, row)
	} else if rowData.Status == "Failed" {
		hostip := reporting.ChangeColour("red", rowData.Hostip)
		parameter := reporting.ChangeColour("red", rowData.Parameter)
		status := reporting.ChangeColour("red", reporting.AppendSpecialCharater(1, rowData.Status))
		message := reporting.ChangeColour("red", rowData.StatusMessage.MainMessage)

		row = table.Row{index, hostip, parameter, status, message}
		rows = append(rows, row)
	}

	if len(rowData.StatusMessage.SubMessage) > 0 {
		for _, value := range rowData.StatusMessage.SubMessage {
			var newSubMessage string
			if rowData.Status == "Failed" {
				key := reporting.AppendSpecialCharater(1, reporting.AppendSpecialCharater(4, rowData.Status))
				newSubMessage = reporting.ChangeColour("red", key+value)
			} else {
				key := reporting.ChangeColour("yellow", reporting.AppendSpecialCharater(3, reporting.AppendSpecialCharater(4, "Waring")))
				newSubMessage = key + value
			}
			row = table.Row{"", "", "", "", newSubMessage}
			rows = append(rows, row)
		}
	}

	return rows
}

func createSummaryTableRows(rowData map[string]SummaryInfo, reporting Reporting) []table.Row {
	var row table.Row
	var rows []table.Row
	for key, value := range rowData {
		parameter := key
		successCount := reporting.ChangeColour("green", strconv.Itoa(value.SuccessfulCount))
		failedCount := reporting.ChangeColour("red", strconv.Itoa(value.FailedCount))
		toResolve := value.ToResolve
		if len(toResolve) > 1 {
			row = table.Row{parameter, successCount, failedCount, toResolve[0]}
			rows = append(rows, row)
			for _, value := range toResolve[1:] {
				row = table.Row{"", "", "", value}
				rows = append(rows, row)
			}
		} else if len(toResolve) > 0 {
			row = table.Row{parameter, successCount, failedCount, toResolve[0]}
			rows = append(rows, row)
		} else {
			row = table.Row{parameter, successCount, failedCount, ""}
			rows = append(rows, row)
		}
	}
	return rows
}

func createSummaryTableData(summary map[string]SummaryInfo, rowData *Info) map[string]SummaryInfo {
	var successCount, failedCount int
	if _, ok := summary[rowData.Parameter]; ok {
		// key is present in the map
		singleParameterSummary := summary[rowData.Parameter]
		if rowData.Status == "Success" {
			singleParameterSummary.SuccessfulCount++
		} else {
			singleParameterSummary.FailedCount++
		}

		singleParameterSummary.ToResolve = append(singleParameterSummary.ToResolve, rowData.StatusMessage.ToResolve...)
		summary[rowData.Parameter] = singleParameterSummary
		return summary
	}
	if rowData.Status == "Success" {
		successCount++
	} else {
		failedCount++
	}

	summary[rowData.Parameter] = SummaryInfo{
		SuccessfulCount: successCount,
		FailedCount:     failedCount,
		ToResolve:       rowData.StatusMessage.ToResolve,
	}
	return summary
}
