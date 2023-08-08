package licenseaudit

import (
	"context"

	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/teambition/rrule-go"
)

func createOrUpdateWorkflowSchedule(cerealManager *cereal.Manager, scheduleName string, auditWorkflowName cereal.WorkflowName, rule *rrule.RRule) error {
	err := cerealManager.CreateWorkflowSchedule(context.Background(), scheduleName, auditWorkflowName, nil, true, rule)

	if err == nil {
		return nil
	}

	if err == cereal.ErrWorkflowScheduleExists {
		log.Infof("license-audit workflow %s already exists, not creating", scheduleName)

	} else {
		return errors.Wrapf(err, "could not continue creating license-audit workflow schedule %s", scheduleName)
	}
	return nil
}
