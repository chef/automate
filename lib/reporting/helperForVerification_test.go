package reporting

import (
	"fmt"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/fatih/color"
	"github.com/jedib0t/go-pretty/v5/table"
	"github.com/stretchr/testify/assert"
)

const (
	automate     = "Automate"
	certificate  = "Certificates"
	toResolveMsg = "1. Add the start and end certificate markers"
	summaryTable = "AutomateSummaryTable"
	ip           = "127.0.0.4"
	ipResult     = "127.0.0.4:"
)

var tbMap = map[string]*Table{
	automate: {
		Title:     automate,
		Header:    table.Row{"No.", "Identifier", "Parameter", "Status", "Message"},
		ColConfig: []table.ColumnConfig{{Number: 1, WidthMax: 5, WidthMin: 5}, {Number: 2, WidthMax: 15, WidthMin: 15}, {Number: 3, WidthMax: 25, WidthMin: 25}, {Number: 4, WidthMax: 15, WidthMin: 15}, {Number: 5, WidthMax: 60, WidthMin: 60}},
	},
	summaryTable: {
		Title:     "SUMMARY : Automate",
		Header:    table.Row{"Parameter", "Successful", "Failed", "How to resolve it"},
		ColConfig: []table.ColumnConfig{{Number: 1, WidthMax: 30, WidthMin: 30}, {Number: 2, WidthMax: 15, WidthMin: 15}, {Number: 3, WidthMax: 15, WidthMin: 15}, {Number: 4, WidthMax: 65, WidthMin: 65}},
	},
}

func getMockReportingModule(wr *cli.Writer) Reporting {
	return &ReportingModule{
		writer: wr,
		tables: tbMap,
	}
}

var nodeInfoMap = make(map[string][]Info)

func TestVerificationReports(t *testing.T) {
	cw := getMockWriterImpl()
	type args struct {
		reports     []VerificationReport
		reporting   Reporting
		nodeInfoMap map[string][]Info
	}

	test := struct {
		name string
		args args
		want string
	}{

		name: successStr,
		args: args{
			reports: []VerificationReport{
				{
					TableKey: "",
					Report: Info{
						Hostip:        "",
						Parameter:     "",
						Status:        "",
						StatusMessage: &StatusMessage{},
						SummaryInfo:   &SummaryInfo{},
					},
				},
			},
			reporting:   getMockReportingModule(cw.CliWriter),
			nodeInfoMap: nodeInfoMap,
		},
		want: "Automate\n+-------+-----------------+---------------------------+-----------------+--------------------------------------------------------------+\n|   NO. |      IDENTIFIER |                 PARAMETER |          STATUS |                                                      MESSAGE |\n+-------+-----------------+---------------------------+-----------------+--------------------------------------------------------------+\n+-------+-----------------+---------------------------+-----------------+--------------------------------------------------------------+\nSUMMARY : Automate\n+--------------------------------+-----------------+-----------------+-------------------------------------------------------------------+\n|                      PARAMETER |      SUCCESSFUL |          FAILED |                                                 HOW TO RESOLVE IT |\n+--------------------------------+-----------------+-----------------+-------------------------------------------------------------------+\n+--------------------------------+-----------------+-----------------+-------------------------------------------------------------------+\n",
	}

	VerificationReports(test.args.reports, test.args.reporting, test.args.nodeInfoMap)

	fmt.Println("output: ", cw.Output())
	assert.Equal(t, test.want, cw.Output())

}

func TestCreateStatusTableRows(t *testing.T) {
	type args struct {
		index     int
		rowData   *Info
		reporting Reporting
	}
	tests := []struct {
		name string
		args args
		want []table.Row
	}{
		{
			name: "Status is Success",
			args: args{
				index: 1,
				rowData: &Info{
					Hostip:    ip,
					Parameter: certificate,
					Status:    successStr,
					StatusMessage: &StatusMessage{
						MainMessage: "Cerificate validation successful",
						SubMessage:  []string{"Certificate about to expire <Date>"},
					},
					SummaryInfo: &SummaryInfo{},
				},
				reporting: getMockReportingModule(getMockWriterImpl().CliWriter),
			},
			want: []table.Row{{1, ip, certificate, color.New(color.FgGreen).Sprint("✔ Success"), "Cerificate validation successful"}, {"", "", "", "", color.New(color.FgYellow).Sprint("! [ Warning ] ") + "Certificate about to expire <Date>"}},
		},
		{
			name: "Status is Failed",
			args: args{
				index: 1,
				rowData: &Info{
					Hostip:    ip,
					Parameter: certificate,
					Status:    failedStr,
					StatusMessage: &StatusMessage{
						MainMessage: "Certificate validation failed",
						SubMessage:  []string{"Certificate is not formatted properly"},
					},
					SummaryInfo: &SummaryInfo{},
				},
				reporting: getMockReportingModule(getMockWriterImpl().CliWriter),
			},
			want: []table.Row{{1, color.New(color.FgRed).Sprint(ip), color.New(color.FgRed).Sprint(certificate), color.New(color.FgRed).Sprint("✖ Failed"), color.New(color.FgRed).Sprint("Certificate validation failed")}, {"", "", "", "", color.New(color.FgRed).Sprint("✖ [ Failed ] Certificate is not formatted properly")}},
		},
		{
			name: "Status is Skipped",
			args: args{
				index: 1,
				rowData: &Info{
					Hostip:    ip,
					Parameter: certificate,
					Status:    skippedStr,
					StatusMessage: &StatusMessage{
						MainMessage: "Certificate validation skipped",
					},
					SummaryInfo: &SummaryInfo{},
				},
				reporting: getMockReportingModule(getMockWriterImpl().CliWriter),
			},
			want: []table.Row{{1, ip, certificate, color.New(90).Sprint("⊖ Skipped"), "Certificate validation skipped"}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := createStatusTableRows(tt.args.index, tt.args.rowData, tt.args.reporting)
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestCreateSummaryTableRowData(t *testing.T) {
	type args struct {
		summary map[string]SummaryInfo
		rowData *Info
	}
	tests := []struct {
		name string
		args args
		want map[string]SummaryInfo
	}{
		{
			name: successStr,
			args: args{
				summary: map[string]SummaryInfo{},
				rowData: &Info{
					Hostip:    ip,
					Parameter: certificate,
					Status:    failedStr,
					StatusMessage: &StatusMessage{
						MainMessage: "Certificate B validation failed",
						SubMessage:  []string{"Certificate is not formatted properly"},
					},
					SummaryInfo: &SummaryInfo{
						FailedCount: 1,
						ToResolve:   []string{"Add the start and end certificate markers"},
					},
				},
			},
			want: map[string]SummaryInfo{
				certificate: {
					SuccessfulCount: 0,
					FailedCount:     1,
					ToResolve:       []string{ipResult, toResolveMsg},
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := createSummaryTableRowData(tt.args.summary, tt.args.rowData)
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestCreateSummaryTableRows(t *testing.T) {
	type args struct {
		rowData   map[string]SummaryInfo
		reporting Reporting
	}
	tests := []struct {
		name string
		args args
		want []table.Row
	}{
		{
			name: "ToResolve steps are present",
			args: args{
				rowData: map[string]SummaryInfo{
					certificate: {
						SuccessfulCount: 0,
						FailedCount:     1,
						ToResolve:       []string{ipResult, toResolveMsg},
					},
				},
				reporting: getMockReportingModule(getMockWriterImpl().CliWriter),
			},
			want: []table.Row{{"Certificates", color.New(color.FgGreen).Sprint("0"), color.New(color.FgRed).Sprint("1"), ipResult}, {"", "", "", toResolveMsg}},
		},
		{
			name: "ToResolve steps are not present",
			args: args{
				rowData: map[string]SummaryInfo{
					certificate: {
						SuccessfulCount: 1,
						FailedCount:     0,
						ToResolve:       []string{},
					},
				},
				reporting: getMockReportingModule(getMockWriterImpl().CliWriter),
			},
			want: []table.Row{{"Certificates", color.New(color.FgGreen).Sprint("1"), color.New(color.FgRed).Sprint("0"), ""}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := createSummaryTableRows(tt.args.rowData, tt.args.reporting)
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestUpdateTableTitle(t *testing.T) {
	type args struct {
		reporting Reporting
		nodeInfo  map[string][]Info
		keyMap    map[string][]string
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		{
			name: "Title updated correctly when status is failed",
			args: args{
				reporting: getMockReportingModule(getMockWriterImpl().CliWriter),
				nodeInfo: map[string][]Info{
					automate: {
						{
							Hostip:    ip,
							Parameter: certificate,
							Status:    failedStr,
						},
					},
				},
				keyMap: map[string][]string{
					automate: {automate, summaryTable},
				},
			},
			want: color.New(color.FgRed).Sprint("✖ Automate"),
		},
		{
			name: "Title updated correctly when status is succes",
			args: args{
				reporting: getMockReportingModule(getMockWriterImpl().CliWriter),
				nodeInfo: map[string][]Info{
					automate: {
						{
							Hostip:    ip,
							Parameter: certificate,
							Status:    successStr,
						},
					},
				},
				keyMap: map[string][]string{
					automate: {automate, summaryTable},
				},
			},
			want: color.New(color.FgGreen).Sprint("✔ Automate"),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			oldTitle := tt.args.reporting.GetTable(tt.args.keyMap[automate][0]).Title
			updateTableTitle(tt.args.reporting, tt.args.nodeInfo, tt.args.keyMap)
			assert.Equal(t, tt.want, tt.args.reporting.GetTable(tt.args.keyMap[automate][0]).Title)
			tt.args.reporting.GetTable(tt.args.keyMap[automate][0]).Title = oldTitle
		})
	}
}

func TestCheckFailedStatus(t *testing.T) {
	type args struct {
		info []Info
	}
	tests := []struct {
		name string
		args args
		want bool
	}{
		{
			name: "Info array contains report with failed status",
			args: args{
				info: []Info{
					{Status: failedStr},
					{Status: successStr},
					{Status: successStr},
				},
			},
			want: true,
		},
		{
			name: "Info array does not contains report with failed status",
			args: args{
				info: []Info{
					{Status: successStr},
					{Status: successStr},
					{Status: successStr},
				},
			},
			want: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := checkFailedStatus(tt.args.info); got != tt.want {
				t.Errorf("checkFailedStatus() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestMatchNodeNameWithRespectiveTablesName(t *testing.T) {
	type args struct {
		substring string
		keys      []string
	}
	test := struct {
		name string
		args args
		want []string
	}{
		name: "Keys containing substring in alphabetical order",
		args: args{
			substring: "ab",
			keys:      []string{"abab", "abc", "bcd", "bcda"},
		},
		want: []string{"abab", "abc"},
	}

	got := matchNodeNameWithRespectiveTablesName(test.args.substring, test.args.keys)
	assert.Equal(t, test.want, got)
}

func TestCreateIndexForToResolve(t *testing.T) {
	type args struct {
		data []string
	}
	test := struct {
		name string
		args args
		want []string
	}{
		name: successStr,
		args: args{
			data: []string{"Step 1", "Step 2"},
		},
		want: []string{"1. Step 1", "2. Step 2"},
	}

	got := createIndexForToResolve(test.args.data)
	assert.Equal(t, test.want, got)

}
