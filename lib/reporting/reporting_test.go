package reporting

import (
	"reflect"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/fatih/color"
	"github.com/jedib0t/go-pretty/v5/table"
	"github.com/stretchr/testify/assert"
)

const (
	successStr = "Success"
	skippedStr = "Skipped"
	failedStr  = "Failed"
	waringStr  = "Warning"
	test1      = "test1"
	test2      = "test2"
	header     = "header"
	header1    = "header1"
	header2    = "header2"
	col        = "col"
	col1       = "col1"
	col2       = "col2"
)

type fields struct {
	writer *cli.Writer
	tables map[string]*Table
}

var tableMap = map[string]*Table{
	test1: {
		Title:     test1,
		Rows:      []table.Row{{col}},
		Header:    table.Row{header},
		Footer:    table.Row{},
		ColConfig: []table.ColumnConfig{},
	},
	test2: {
		Title:     test2,
		Rows:      []table.Row{{col1, col2}},
		Header:    table.Row{header1, header2},
		Footer:    table.Row{},
		ColConfig: []table.ColumnConfig{},
	},
}

var module = fields{
	writer: getMockWriterImpl().CliWriter,
	tables: tableMap,
}

func TestNewReportingModule(t *testing.T) {
	tests := []struct {
		name string
		args fields
		want Reporting
	}{
		{
			name: successStr,
			args: module,
			want: nil,
		},
	}
	for _, tt := range tests {
		NewReportingModule(tt.args.writer, tt.args.tables)
	}
}

func TestGetTable(t *testing.T) {
	tests := []struct {
		name   string
		fields fields
		args   string
		want   *Table
	}{
		{
			name:   successStr,
			fields: module,
			args:   test1,
			want: &Table{
				Title:     test1,
				Rows:      []table.Row{{col}},
				Header:    table.Row{header},
				Footer:    table.Row{},
				ColConfig: []table.ColumnConfig{},
			},
		},
	}
	for _, tt := range tests {
		r := &ReportingModule{
			writer: tt.fields.writer,
			tables: tt.fields.tables,
		}
		got := r.GetTable(tt.args)
		assert.Equal(t, tt.want, got)
	}
}

func TestGetAllTables(t *testing.T) {
	tests := []struct {
		name   string
		fields fields
		want   map[string]*Table
	}{
		{
			name:   successStr,
			fields: module,
			want: map[string]*Table{
				test1: {
					Title:     test1,
					Rows:      []table.Row{{col}},
					Header:    table.Row{header},
					Footer:    table.Row{},
					ColConfig: []table.ColumnConfig{},
				},
				test2: {
					Title:     test2,
					Rows:      []table.Row{{col1, col2}},
					Header:    table.Row{header1, header2},
					Footer:    table.Row{},
					ColConfig: []table.ColumnConfig{},
				}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			r := &ReportingModule{
				writer: tt.fields.writer,
				tables: tt.fields.tables,
			}
			if got := r.GetAllTables(); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("ReportingModule.GetAllTables() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGetAllTableKeys(t *testing.T) {
	test := struct {
		name   string
		fields fields
		want   []string
	}{

		name:   successStr,
		fields: module,
		want:   []string{test1, test2},
	}

	r := &ReportingModule{
		writer: test.fields.writer,
		tables: test.fields.tables,
	}
	got := r.GetAllTableKeys()
	assert.Equal(t, test.want, got)

}

func TestSetTable(t *testing.T) {
	testTableMap := map[string]*Table{
		test1: {
			Title:     test1,
			Rows:      []table.Row{{col}},
			Header:    table.Row{header},
			Footer:    table.Row{},
			ColConfig: []table.ColumnConfig{},
		},
		test2: {
			Title:     test2,
			Rows:      []table.Row{{col1, col2}},
			Header:    table.Row{header1, header2},
			Footer:    table.Row{},
			ColConfig: []table.ColumnConfig{},
		},
	}
	type args struct {
		key   string
		table *Table
	}
	test := struct {
		name   string
		fields fields
		args   args
		want   *Table
	}{
		name: "Replace old table with given table for given key",
		fields: fields{
			writer: getMockWriterImpl().CliWriter,
			tables: testTableMap,
		},
		args: args{
			key:   test1,
			table: testTableMap[test2],
		},
		want: testTableMap[test2],
	}

	r := &ReportingModule{
		writer: test.fields.writer,
		tables: test.fields.tables,
	}
	r.SetTable(test.args.key, test.args.table)
	assert.EqualValues(t, test.want, r.GetTable(test1))
}

func TestAppendSpecialCharater(t *testing.T) {
	type args struct {
		symbol int
		str    string
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   string
	}{
		{
			name:   "Append x",
			fields: module,
			args: args{
				symbol: Failed,
				str:    failedStr,
			},
			want: "✖ Failed",
		},
		{
			name:   "Append ✔",
			fields: module,
			args: args{
				symbol: Success,
				str:    successStr,
			},
			want: "✔ Success",
		},
		{
			name:   "Append !",
			fields: module,
			args: args{
				symbol: Warning,
				str:    waringStr,
			},
			want: "! Warning",
		},
		{
			name:   "Add [] bracket around text",
			fields: module,
			args: args{
				symbol: SquareBracket,
				str:    waringStr,
			},
			want: "[ Warning ] ",
		},
		{
			name:   "String returned as it is",
			fields: module,
			args: args{
				symbol: 5,
				str:    waringStr,
			},
			want: waringStr,
		},
		{
			name:   "Skipped Returnd",
			fields: module,
			args: args{
				symbol: 4,
				str:    waringStr,
			},
			want: "⊖ Warning",
		},
	}
	for _, tt := range tests {
		r := &ReportingModule{
			writer: tt.fields.writer,
			tables: tt.fields.tables,
		}
		got := r.AppendSpecialCharater(tt.args.symbol, tt.args.str)
		assert.Equal(t, tt.want, got)
	}
}

func TestChangeColour(t *testing.T) {
	type args struct {
		fgColor int
		msg     string
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   string
	}{
		{
			name:   "Change color of string to red",
			fields: module,
			args: args{
				fgColor: Red,
				msg:     test1,
			},
			want: color.New(color.FgRed).Sprint(test1),
		},
		{
			name:   "Change color of string to green",
			fields: module,
			args: args{
				fgColor: Green,
				msg:     test1,
			},
			want: color.New(color.FgGreen).Sprint(test1),
		},
		{
			name:   "Change color of string to yellow",
			fields: module,
			args: args{
				fgColor: Yellow,
				msg:     test1,
			},
			want: color.New(color.FgYellow).Sprint(test1),
		},
		{
			name:   "Returned string is same as input",
			fields: module,
			args: args{
				fgColor: 4,
				msg:     test1,
			},
			want: test1,
		},
	}
	for _, tt := range tests {
		r := &ReportingModule{
			writer: tt.fields.writer,
			tables: tt.fields.tables,
		}
		got := r.ChangeColour(tt.args.fgColor, tt.args.msg)
		assert.Equal(t, tt.want, got)
	}
}

func TestGenerateTableOutputAndPrint(t *testing.T) {
	cw := getMockWriterImpl()
	type args struct {
		tb *Table
	}

	test := struct {
		name   string
		fields fields
		args   args
		want   string
	}{
		name: "Print  given table",
		fields: fields{
			writer: cw.CliWriter,
			tables: tableMap,
		},
		args: args{
			tb: tableMap[test1],
		},
		want: "test1\n+--------+\n| HEADER |\n+--------+\n| col    |\n+--------+\n",
	}

	r := &ReportingModule{
		writer: test.fields.writer,
		tables: test.fields.tables,
	}
	r.GenerateTableOutputAndPrint(test.args.tb)
	assert.Equal(t, test.want, cw.Output())
}

func getMockWriterImpl() *majorupgrade_utils.CustomWriter {
	return majorupgrade_utils.NewCustomWriter()
}
