package inspec

const (
	ResultStatusPassed    string = "passed"
	ResultStatusSkipped   string = "skipped"
	ResultStatusFailed    string = "failed"
	ResultStatusLoaded    string = "loaded"
	ControlImpactMinor    string = "minor"
	ControlImpactMajor    string = "major"
	ControlImpactCritical string = "critical"
)

// Status calculates the overall status of all controls based on all results
func (control *Control) Status() (status string) {
	status = ResultStatusPassed
	for _, result := range control.Results {
		if result.Status == ResultStatusFailed {
			status = ResultStatusFailed
			break
		} else if result.Status == ResultStatusSkipped {
			status = ResultStatusSkipped
		}
	}
	return status
}

// ImpactName returns a human readable name for the impact
func (control *Control) ImpactName() (impact string) {
	impact = ControlImpactCritical
	if control.Impact < 0.4 {
		impact = ControlImpactMinor
	} else if control.Impact < 0.7 {
		impact = ControlImpactMajor
	}
	return impact
}
