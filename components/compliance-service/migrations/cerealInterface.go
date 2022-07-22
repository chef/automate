package migrations

import (
	"context"
	"fmt"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type cerealInterface interface {
	EnqueueWorkflowDayLatest(status bool) error
	EnqueueWorkflowControl(status bool) error
}

type cerealService struct {
	cerealManger *cereal.Manager
}

//EnqueueWorkflowDayLatest the message for Day latest flag
func (u *cerealService) EnqueueWorkflowDayLatest(status bool) error {
	var err error
	if status {
		err = u.cerealManger.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
			fmt.Sprintf("%s-%s", MigrationWorkflowName, UpgradeTaskName),
			MigrationWorkflowParameters{
				DayLatestFlag: status,
			})
		if err != nil {
			logrus.Debugf("Unable to Enqueue Workflow for Control Task")
			return errors.Wrapf(err, "Unable to Enqueue Workflow for Control Task")
		}
	}

	return err

}

//EnqueueWorkflowControl enqueue the message for control index flag
func (u *cerealService) EnqueueWorkflowControl(status bool) error {
	var err error
	if !status {
		err = u.cerealManger.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
			fmt.Sprintf("%s-%s", MigrationWorkflowName, UpgradeTaskName),
			MigrationWorkflowParameters{
				ControlIndexFlag: status,
			})
		if err != nil {
			logrus.Debugf("Unable to Enqueue Workflow for Control Task")
			return errors.Wrapf(err, "Unable to Enqueue Workflow for Control Task")
		}
	}

	return err

}
