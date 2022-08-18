package migrations

import "github.com/pkg/errors"

type CerealInterfaceTest struct {
	NeedError           bool
	NeedErrorForControl bool
}

func (c CerealInterfaceTest) EnqueueWorkflowUpgrade(dayLatest bool, controlIndex bool, compRunInfo bool) error {
	if c.NeedError {
		return errors.New("Unable to enqueue workflow for day latest flag")
	}

	return nil

}

func (c CerealInterfaceTest) EnqueueWorkflowDayLatest(status bool) error {
	if c.NeedError {
		return errors.New("Unable to enqueue workflow for day latest flag")
	}

	return nil
}

func (c CerealInterfaceTest) EnqueueWorkflowControl(status bool) error {
	if c.NeedErrorForControl {
		return errors.New("Unable to enqueue workflow for control index flag")
	}

	return nil
}
