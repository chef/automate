package profiles

import (
	"github.com/chef/automate/components/compliance-service/api/profiles"
	"github.com/chef/automate/components/compliance-service/inspec"
)

func inspecCheckResultToGrpcCheckResult(inspecCheckResult inspec.CheckResult) (checkResult profiles.CheckResult) {
	resultSummary := &profiles.ResultSummary{}
	resultSummary.Controls = int32(inspecCheckResult.Summary.Controls)
	resultSummary.Location = inspecCheckResult.Summary.Location
	resultSummary.Timestamp = inspecCheckResult.Summary.Timestamp
	resultSummary.Valid = inspecCheckResult.Summary.Valid

	checkResult.Summary = resultSummary

	var checkMessageErrors = make([]*profiles.CheckMessage, 0)
	for _, inspecCheckMessage := range inspecCheckResult.Errors {
		msg := inspecCheckMessageToGrpcCheckMessage(inspecCheckMessage)
		checkMessageErrors = append(checkMessageErrors, &msg)
	}
	checkResult.Errors = checkMessageErrors

	var checkMessageWarnings = make([]*profiles.CheckMessage, 0)
	for _, inspecCheckMessage := range inspecCheckResult.Warnings {
		msg := inspecCheckMessageToGrpcCheckMessage(inspecCheckMessage)
		checkMessageWarnings = append(checkMessageWarnings, &msg)
	}
	checkResult.Warnings = checkMessageWarnings

	return
}

func inspecCheckMessageToGrpcCheckMessage(inspecCheckMessage inspec.CheckMessage) (checkMessage profiles.CheckMessage) {
	if inspecCheckMessage.Column != nil {
		checkMessage.Column = int32(*inspecCheckMessage.Column)
	}
	checkMessage.ControlId = inspecCheckMessage.ControlId
	checkMessage.File = inspecCheckMessage.File
	if inspecCheckMessage.Line != nil {
		checkMessage.Line = int32(*inspecCheckMessage.Line)
	}
	checkMessage.Msg = inspecCheckMessage.Msg

	return
}
