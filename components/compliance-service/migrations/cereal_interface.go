package migrations

import (
	"context"
	"fmt"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type cerealInterface interface {
	EnqueueWorkflowUpgrade(controlIndex bool) error
}

type cerealService struct {
	cerealManger *cereal.Manager
}

//EnqueueWorkflowDayLatest the message for Day latest flag
func (u *cerealService) EnqueueWorkflowUpgrade(controlIndex bool) error {
	var err error
	if controlIndex {
		err = u.cerealManger.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
			fmt.Sprintf("%s-%s", MigrationWorkflowName, UpgradeTaskName),
			MigrationWorkflowParameters{
				ControlIndexFlag: controlIndex,
			})
		if err != nil {
			logrus.Debugf("Unable to Enqueue Workflow for Daily Latest Task")
			return errors.Wrapf(err, "Unable to Enqueue Workflow for Daily Latest Task")
		}
	}

	return err

}
