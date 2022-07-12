package processor

import (
	"testing"

	"github.com/chef/automate/components/compliance-service/reporting/relaxting"
	"github.com/stretchr/testify/require"
)

const (
	UUID  = "efd46e48-5751-40a2-a705-1182356908c1"
	ID    = "3q09ru4orbfer k vfksd "
	Title = "some different tile"
	Env   = "Test Env"
)

func TestMapStructs(t *testing.T) {
	type args struct {
		inspecReport *relaxting.ESInSpecReport
	}
	tests := []struct {
		name    string
		args    args
		want    []relaxting.Control
		wantErr bool
	}{
		{
			name: "Test Empty",
			args: args{
				inspecReport: &relaxting.ESInSpecReport{},
			},
			want:    []relaxting.Control{},
			wantErr: false,
		},
		{
			name: "Test with Value",
			args: args{
				inspecReport: &relaxting.ESInSpecReport{
					NodeID:      "ssdpweoru4etu5hgsklvldfknv",
					ReportID:    UUID,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "on",
					Environment: Env,
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
									ID:         ID,
									Impact:     0,
									Title:      Title,
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
			want: []relaxting.Control{
				{
					ControlID:   "adcksnjvfskhvbsfk",
					Title:       "string",
					WaivedStr:   "no",
					WaiverData:  (*relaxting.ESInSpecReportControlsWaiverData)(nil),
					Impact:      0,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "on",
					Nodes:       []relaxting.Node{relaxting.Node{NodeUUID: "ssdpweoru4etu5hgsklvldfknv", Status: "on", DayLatest: true, DailyLatest: true, ReportUUID: UUID}},
					Profile:     relaxting.Profile{ProfileID: ""},
				},
				{
					ControlID:   ID,
					Title:       Title,
					WaivedStr:   "no",
					WaiverData:  (*relaxting.ESInSpecReportControlsWaiverData)(nil),
					Impact:      0,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "failed",
					Nodes:       []relaxting.Node{relaxting.Node{NodeUUID: "ssdpweoru4etu5hgsklvldfknv", Status: "on", DayLatest: true, DailyLatest: true, ReportUUID: UUID}},
					Profile:     relaxting.Profile{ProfileID: ""},
				},
			},
			wantErr: false,
		},
		{
			name: "Test for Enviroment",
			args: args{
				inspecReport: &relaxting.ESInSpecReport{
					NodeID:      "ssdpweoru4etu5hgsklvldfknv",
					ReportID:    UUID,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "on",
					Environment: Env,
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name: "",
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:         ID,
									Impact:     0,
									Title:      Title,
									Status:     "failed",
									Results:    nil,
									WaiverData: &relaxting.ESInSpecReportControlsWaiverData{Run: true},
									WaivedStr:  "yes",
								},
							},
						},
					},
				},
			},

			want: []relaxting.Control{
				{
					ControlID:   ID,
					Title:       Title,
					WaivedStr:   "yes",
					WaiverData:  &relaxting.ESInSpecReportControlsWaiverData{Run: true},
					Impact:      0,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "waived",
					Nodes:       []relaxting.Node{relaxting.Node{NodeUUID: "ssdpweoru4etu5hgsklvldfknv", Status: "on", DayLatest: true, DailyLatest: true, ReportUUID: UUID, Environment: Env}},
					Profile:     relaxting.Profile{ProfileID: ""},
				},
			},
			wantErr: false,
		},
		{
			name: "Test for Enviroment",
			args: args{
				inspecReport: &relaxting.ESInSpecReport{
					NodeID:      "ssdpweoru4etu5hgsklvldfknv",
					ReportID:    UUID,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "on",
					Environment: Env,
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name: "",
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:         ID,
									Impact:     0,
									Title:      Title,
									Status:     "failed",
									Results:    nil,
									WaiverData: &relaxting.ESInSpecReportControlsWaiverData{Run: true},
									WaivedStr:  "yes",
								},
							},
						},
					},
				},
			},

			want: []relaxting.Control{
				{
					ControlID:   ID,
					Title:       Title,
					WaivedStr:   "yes",
					WaiverData:  &relaxting.ESInSpecReportControlsWaiverData{Run: true},
					Impact:      0,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "waived",
					Nodes:       []relaxting.Node{relaxting.Node{NodeUUID: "ssdpweoru4etu5hgsklvldfknv", Status: "on", DayLatest: true, DailyLatest: true, ReportUUID: UUID, Environment: Env}},
					Profile:     relaxting.Profile{ProfileID: ""},
				},
			},
			wantErr: false,
		},
		{
			name: "Test for Policy",
			args: args{
				inspecReport: &relaxting.ESInSpecReport{
					NodeID:      "ssdpweoru4etu5hgsklvldfknv",
					ReportID:    UUID,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "on",
					PolicyName:  "Test Policy",
					Profiles: []relaxting.ESInSpecReportProfile{
						{
							Name: "",
							Controls: []relaxting.ESInSpecReportControl{
								{
									ID:         ID,
									Impact:     0,
									Title:      Title,
									Status:     "failed",
									Results:    nil,
									WaiverData: &relaxting.ESInSpecReportControlsWaiverData{Run: true},
									WaivedStr:  "yes",
								},
							},
						},
					},
				},
			},
			want: []relaxting.Control{
				{
					ControlID:   ID,
					Title:       Title,
					WaivedStr:   "yes",
					WaiverData:  &relaxting.ESInSpecReportControlsWaiverData{Run: true},
					Impact:      0,
					DailyLatest: true,
					DayLatest:   true,
					Status:      "waived",
					Nodes:       []relaxting.Node{relaxting.Node{NodeUUID: "ssdpweoru4etu5hgsklvldfknv", Status: "on", DayLatest: true, DailyLatest: true, ReportUUID: UUID, PolicyName: "Test Policy"}},
					Profile:     relaxting.Profile{ProfileID: ""},
				},
			},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := MapStructsESInSpecReportToControls(tt.args.inspecReport)
			if tt.name == "Test 1" {
				require.NoError(t, err)
				require.Empty(t, got)
			}

			if tt.name == "Test with Value" {
				require.NoError(t, err)
				require.NotEmpty(t, got)
				require.Equal(t, got, tt.want)
			}

			if tt.name == "Test for waived status" {
				require.NoError(t, err)
				require.NotEmpty(t, got)
				require.Equal(t, got, tt.want)
			}
		})
	}
}
