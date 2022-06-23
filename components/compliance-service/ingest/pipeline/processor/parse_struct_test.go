package processor

import (
	"fmt"
	"testing"

	"github.com/chef/automate/components/compliance-service/integration_test"
	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestParseReportCtrlStruct(t *testing.T) {
	suit := integration_test.NewGlobalSuite()

	inspecReport := relaxting.ESInSpecReport{ReportID: "efd46e48-5751-40a2-a705-1182356908c1"}
	controls, err := ParseReportCtrlStruct(suit.ingesticESClient, &inspecReport, "comp-7-r-2022.06.22")
	assert.NoError(t, err)
	assert.Nil(t, controls)
}

func TestMapStructs(t *testing.T) {
	type args struct {
		inspecReport *relaxting.ESInSpecReport
	}
	tests := []struct {
		name    string
		args    args
		want    []Control
		wantErr bool
	}{
		{
			name: "Test Empty",
			args: args{
				inspecReport: &relaxting.ESInSpecReport{},
			},
			want:    []Control{},
			wantErr: false,
		},
		{
			name: "Test with Value",
			args: args{
				inspecReport: &relaxting.ESInSpecReport{
					NodeID:      "",
					ReportID:    "efd46e48-5751-40a2-a705-1182356908c1",
					DailyLatest: true,
					DayLatest:   true,
					Status:      "on",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name: "",
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:         "adcksnjvfskhvbsfk",
									Impact:     0,
									Title:      "string",
									Status:     "on",
									Results:    nil,
									WaiverData: nil,
									WaivedStr:  "no",
								},
								{
									ID:         "3q09ru4orbfer k vfksd ",
									Impact:     0,
									Title:      "some different tile",
									Status:     "failed",
									Results:    nil,
									WaiverData: nil,
									WaivedStr:  "no",
								},
							},
						},
					},
				},
			},
			want: []Control{
				{
					ControlID:   "adcksnjvfskhvbsfk",
					Title:       "string",
					WaivedStr:   "no",
					WaiverData:  (*relaxting.ESInSpecReportControlsWaiverData)(nil),
					Impact:      0,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "on",
					Nodes:       Node{NodeUUID: "", Status: "on", DayLatest: true, DailyLatest: true, ReportUUID: "efd46e48-5751-40a2-a705-1182356908c1"},
					Profile:     Profile{ProfileID: ""},
				},
				{
					ControlID:   "3q09ru4orbfer k vfksd ",
					Title:       "some different tile",
					WaivedStr:   "no",
					WaiverData:  (*relaxting.ESInSpecReportControlsWaiverData)(nil),
					Impact:      0,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "failed",
					Nodes:       Node{NodeUUID: "", Status: "on", DayLatest: true, DailyLatest: true, ReportUUID: "efd46e48-5751-40a2-a705-1182356908c1"},
					Profile:     Profile{ProfileID: ""},
				},
			},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := MapStructs(tt.args.inspecReport)
			if tt.name == "Test 1" {
				require.NoError(t, err)
				require.Empty(t, got)
			}

			if tt.name == "Test with Value" {
				fmt.Printf("Alpha: %+v", got)
				fmt.Printf("Beta: %+v", err)
				require.NoError(t, err)
				require.NotEmpty(t, got)
				require.Equal(t, got, tt.want)
			}
		})
	}
}
