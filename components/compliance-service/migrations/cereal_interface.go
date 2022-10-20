package migrations

import (
	"context"
	"fmt"
	"time"

	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type cerealInterface interface {
	EnqueueWorkflowUpgrade(updateDate time.Time) error
}

type cerealService struct {
	cerealManger *cereal.Manager
}

//EnqueueWorkflowUpgrade enqueue the work flow
func (u *cerealService) EnqueueWorkflowUpgrade(updateDate time.Time) error {
	err := u.cerealManger.EnqueueWorkflow(context.TODO(), MigrationWorkflowName,
		fmt.Sprintf("%s-%s-%s", MigrationWorkflowName, UpgradeTaskName, time.Now().Format(time.RFC3339)),
		MigrationWorkflowParameters{
			ControlIndexFlag: true,
			UpgradeDate:      updateDate,
		})
	if err != nil {
		logrus.Errorf("Unable to Enqueue Workflow for Daily Latest Task %v", err)
		return errors.Wrapf(err, "Unable to Enqueue Workflow for Daily Latest Task")
	}
	return nil
}
