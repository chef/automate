package migrations

import (
	"github.com/pkg/errors"
	"time"
)

type CerealInterfaceTest struct {
	NeedError           bool
	NeedErrorForControl bool
}

func (c CerealInterfaceTest) EnqueueWorkflowUpgrade(update time.Time) error {
	if c.NeedError {
		return errors.New("Unable to enqueue workflow for day latest flag")
	}

	return nil

}
