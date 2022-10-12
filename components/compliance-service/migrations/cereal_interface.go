package migrations

import (
	"context"
	"fmt"

	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type cerealInterface interface {
	EnqueueWorkflowUpgrade() error
}

type cerealService struct {
	cerealManger *cereal.Manager
}

//EnqueueWorkflowUpgrade enqueue the work flow
func (u *cerealService) EnqueueWorkflowUpgrade() error {
	err := u.cerealManger.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
		fmt.Sprintf("%s-%s", MigrationWorkflowName, UpgradeTaskName),
		MigrationWorkflowParameters{
			ControlIndexFlag: true,
		})
	if err != nil {
		logrus.Debugf("Unable to Enqueue Workflow for Daily Latest Task")
		return errors.Wrapf(err, "Unable to Enqueue Workflow for Daily Latest Task")
	}
	return nil
}
