package migrations

import (
	"context"
	"fmt"
	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type cerealInterface interface {
	EnqueueWorkflowUpgrade(dayLatest bool, controlIndex bool, compRunInfo bool) error
	EnqueueWorkflowControl(status bool) error
}

type cerealService struct {
	cerealManger *cereal.Manager
}

//EnqueueWorkflowDayLatest the message for Day latest flag
func (u *cerealService) EnqueueWorkflowUpgrade(dayLatest bool, controlIndex bool, compRunInfo bool) error {
	var err error
	if dayLatest {
		err = u.cerealManger.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
			fmt.Sprintf("%s-%s", MigrationWorkflowName, UpgradeTaskName),
			MigrationWorkflowParameters{
				DayLatestFlag:    dayLatest,
				ControlIndexFlag: controlIndex,
				CompRunInfoFlag:  compRunInfo,
			})
		if err != nil {
			logrus.Debugf("Unable to Enqueue Workflow for Daily Latest Task")
			return errors.Wrapf(err, "Unable to Enqueue Workflow for Daily Latest Task")
		}
	}

	return err

}

//EnqueueWorkflowControl enqueue the message for control index flag
func (u *cerealService) EnqueueWorkflowControl(status bool) error {
	var err error
	if status {
		err = u.cerealManger.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
			fmt.Sprintf("%s-%s", MigrationWorkflowName, UpgradeTaskName),
			MigrationWorkflowParameters{
				ControlIndexFlag: status,
			})
		if err != nil {
			logrus.Debugf("Unable to Enqueue Workflow for Control Index Task")
			return errors.Wrapf(err, "Unable to Enqueue Workflow for Control Index Task")
		}
	}

	return err

}
