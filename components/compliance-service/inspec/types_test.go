package inspec

import (
	"testing"

	"github.com/chef/automate/components/compliance-service/api/reporting"
	"github.com/stretchr/testify/assert"
)

func TestControlImpact(t *testing.T) {

	assert.Equal(t, 0.1, 0.1, "they should be equal")

	control := Control{ID: "1", Impact: 100}
	assert.Equal(t, float32(100), control.Impact, "impact should be equal")

	for i := 0; i < 11; i++ {
		control = Control{ID: string(i), Impact: float32(i) * 0.1}
		assert.Equal(t, float32(i)*0.1, control.Impact, "impact should be equal")

		name := "minor"
		if i >= 4 && i < 7 {
			name = "major"
		} else if i >= 7 {
			name = "critical"
		}
		assert.Equal(t, name, control.ImpactName(), "impact should be equal")
	}
}

func TestControlStatus(t *testing.T) {

	assert.Equal(t, 0.1, 0.1, "they should be equal")

	control := Control{ID: "1", Results: []*reporting.Result{{Status: ResultStatusPassed}}}
	assert.Equal(t, ResultStatusPassed, control.Status(), "status is composed of individual results")

	control = Control{ID: "1", Results: []*reporting.Result{{Status: ResultStatusFailed}}}
	assert.Equal(t, ResultStatusFailed, control.Status(), "status is composed of individual results")

	control = Control{ID: "1", Results: []*reporting.Result{{Status: ResultStatusSkipped}}}
	assert.Equal(t, ResultStatusSkipped, control.Status(), "status is composed of individual results")

	control = Control{ID: "1", Results: []*reporting.Result{{Status: ResultStatusPassed}, {Status: ResultStatusPassed}}}
	assert.Equal(t, ResultStatusPassed, control.Status(), "if multiple tests pass, control passes")

	control = Control{ID: "1", Results: []*reporting.Result{{Status: ResultStatusPassed}, {Status: ResultStatusFailed}}}
	assert.Equal(t, ResultStatusFailed, control.Status(), "if one result fails, the control is failed")

	// TODO: we may want to have a look at this
	control = Control{ID: "1", Results: []*reporting.Result{{Status: ResultStatusPassed}, {Status: ResultStatusSkipped}}}
	assert.Equal(t, ResultStatusSkipped, control.Status(), "skip overrides pass")

	control = Control{ID: "1", Results: []*reporting.Result{{Status: ResultStatusPassed}, {Status: ResultStatusFailed}, {Status: ResultStatusSkipped}}}
	assert.Equal(t, ResultStatusFailed, control.Status(), "if one result fails, the control is failed")

	control = Control{ID: "1", Results: []*reporting.Result{{Status: ResultStatusFailed}, {Status: ResultStatusSkipped}}}
	assert.Equal(t, ResultStatusFailed, control.Status(), "if one result fails, the control is failed")

}
