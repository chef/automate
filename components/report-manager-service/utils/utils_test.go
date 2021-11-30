package utils_test

import (
	"testing"
	"time"

	"github.com/chef/automate/components/report-manager-service/utils"
	"github.com/stretchr/testify/assert"
)

func TestComputeDuration(t *testing.T) {
	endedAt := time.Now()
	tests := []struct {
		name      string
		startTime time.Time
		endTime   time.Time
		duration  string
	}{
		{
			name:      "duration_0s",
			startTime: endedAt,
			endTime:   endedAt,
			duration:  "0s",
		},
		{
			name:      "duration_1sec",
			startTime: endedAt.Add(-900 * time.Millisecond),
			endTime:   endedAt,
			duration:  "1s",
		},
		{
			name:      "duration_2sec",
			startTime: endedAt.Add(-2400 * time.Millisecond),
			endTime:   endedAt,
			duration:  "2s",
		},
		{
			name:      "duration_5sec",
			startTime: endedAt.Add(-4500 * time.Millisecond),
			endTime:   endedAt,
			duration:  "5s",
		},
		{
			name:      "duration_60sec",
			startTime: endedAt.Add(-60000 * time.Millisecond),
			endTime:   endedAt,
			duration:  "1m",
		},
		{
			name:      "duration_54min",
			startTime: endedAt.Add(-54499 * 60 * time.Millisecond),
			endTime:   endedAt,
			duration:  "54m",
		},
		{
			name:      "duration_55min",
			startTime: endedAt.Add(-54500 * 60 * time.Millisecond),
			endTime:   endedAt,
			duration:  "55m",
		},
		{
			name:      "duration_1h",
			startTime: endedAt.Add(-1000 * 60 * 60 * time.Millisecond),
			endTime:   endedAt,
			duration:  "1H",
		},
		{
			name:      "duration_1h",
			startTime: endedAt.Add(-1450 * 60 * 60 * time.Millisecond),
			endTime:   endedAt,
			duration:  "1H26m",
		},
		{
			name:      "duration_2h",
			startTime: endedAt.Add(-4590 * 60 * 60 * time.Millisecond),
			endTime:   endedAt,
			duration:  "4H35m",
		},
		{
			name:      "duration_0h",
			startTime: endedAt.Add(4 * time.Millisecond),
			endTime:   endedAt,
			duration:  "0s",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result := utils.ComputeDuration(tc.startTime, tc.endTime)
			assert.Equal(t, tc.duration, result)
		})
	}
}

func TestGetObjName(t *testing.T) {
	result := utils.GetObjName("test")
	assert.Equal(t, "test.json", result)
}

func TestGetCSVObjName(t *testing.T) {
	result := utils.GetCSVObjName("test")
	assert.Equal(t, "test.csv", result)
}
