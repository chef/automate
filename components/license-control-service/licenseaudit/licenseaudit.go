package licenseaudit

import (
	"context"

	"github.com/chef/automate/lib/cereal"
	"github.com/pkg/errors"
	"github.com/teambition/rrule-go"
)

func CreateOrUpdateWorkflowSchedule(cerealManager *cereal.Manager, scheduleName string, auditWorkflowName cereal.WorkflowName, rule *rrule.RRule) error {
	err := cerealManager.CreateWorkflowSchedule(context.Background(), scheduleName, auditWorkflowName, nil, true, rule)

	if err == nil {
		return nil
	}

	if err == cereal.ErrWorkflowScheduleExists {
		schedule, err := cerealManager.GetWorkflowScheduleByName(context.Background(), scheduleName, auditWorkflowName)
		if err != nil {
			return errors.Wrapf(err, "failed to get scheduled workflow %s from cereal manager", scheduleName)
		}
		scheduledRule, err := schedule.GetRRule()
		if err != nil {
			return errors.Wrapf(err, "unable to get rrule for scheduled workflow %s", scheduleName)
		}
		if scheduledRule != rule {
			err = cerealManager.UpdateWorkflowScheduleByName(context.Background(), scheduleName, auditWorkflowName, cereal.UpdateRecurrence(rule))
			if err != nil {
				return errors.Wrapf(err, "unable to update recurrence rule for scheduled workflow %s", scheduleName)
			}
		}

	} else {
		return errors.Wrapf(err, "could not continue creating license-audit workflow schedule %s", scheduleName)
	}
	return nil
}
