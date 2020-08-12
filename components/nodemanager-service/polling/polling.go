package polling

import (
	"context"
	"strings"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/compliance/common"
	aEvent "github.com/chef/automate/api/interservice/event"
	event "github.com/chef/automate/components/event-service/config"

	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/components/nodemanager-service/managers"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
)

func CheckManagersStatuses(ctx context.Context, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) error {
	// get all managers
	// note: we really don't expect anyone to have more than 100 nodemanagers, so this is ok for now
	allManagers, _, err := db.GetNodeManagers("", 0, 1, 100, []*common.Filter{})
	if err != nil {
		logrus.Errorf("checkManagersStatuses unable to check for node managers %s", err.Error())
		return err
	}
	// do a dry run connection on all aws and azure managers to ensure creds are still good
	for _, mgr := range allManagers {
		if strings.HasPrefix(mgr.Type, "aws") {
			myaws, err := managers.GetAWSManagerFromCredential(ctx, mgr.CredentialId, db, secretsClient)
			if err != nil {
				logrus.Error(err)
				return err
			}
			err = myaws.TestConnectivity(ctx)
			return handleManagerStatusError(db, mgr, err)
		}
		if strings.HasPrefix(mgr.Type, "azure") {
			myazure, err := managers.GetAzureManagerFromCredential(ctx, mgr.CredentialId, db, secretsClient)
			if err != nil {
				logrus.Error(err)
				return err
			}
			err = myazure.TestConnectivity(ctx)
			return handleManagerStatusError(db, mgr, err)
		}
	}
	return nil
}

func handleManagerStatusError(db *pgdb.DB, mgr *manager.NodeManager, err error) error {
	// if they are not successful, update manager status to be "unreachable"
	if err != nil {
		logrus.Errorf("updating manager %s with status unreachable due to error: %s", mgr.Name, err.Error())
		err := db.UpdateManagerStatus(mgr.Id, "unreachable")
		if err != nil {
			logrus.Errorf("unable to update manager status: %s", err.Error())
			return err
		}
	} else {
		logrus.Infof("Connection check successful for manager: %s", mgr.Name)
		return nil
	}
	return nil
}

func QueryAwsEc2InstanceStates(ctx context.Context, db *pgdb.DB, secretsClient secrets.SecretsServiceClient, eventsClient aEvent.EventServiceClient) error {
	logrus.Infof("processing aws-ec2 instances due for status check...")

	// get all aws-ec2 manager ids
	mgrIds, err := db.GetAllManagersByType("aws-ec2")
	if err != nil {
		logrus.Errorf("queryAwsEc2InstanceStates unable to get node manager ids %s", err.Error())
		return err
	}
	if len(mgrIds) == 0 {
		logrus.Info("no aws-ec2 managers found")
	}

	// GetStateInfoByManager will get manager connection for each manager
	// and call queryStatus for each region for each manager
	for _, mgrId := range mgrIds {
		instanceStatesResp, err := managers.GetStateInfoByManager(ctx, mgrId, db, "aws-ec2", secretsClient)
		if err != nil {
			logrus.Errorf("queryAwsEc2InstanceStates unable to get state information for instances %s", err.Error())
			return err
		}
		// get the account id, a unique identifier
		nodeManager, err := db.GetNodeManager(mgrId)
		if err != nil {
			logrus.Errorf("queryAwsEc2InstanceStates unable to retrieve manager %s", err.Error())
			return err
		}
		// update each instance's source_state; if source_state != running, update status=unreachable
		for _, inst := range instanceStatesResp {

			// We use the updated boolean to determine if a node enters a new state
			// this prevents us from sending out events continuously about already terminated nodes.
			updated, err := db.UpdateOrInsertInstanceSourceStateInDb(inst, mgrId, nodeManager.AccountId, "aws-ec2")
			if inst.State == "terminated" && updated {
				fireEvent(event.NodeTerminatedEventName, inst.ID, eventsClient)
			}
			if err != nil {
				logrus.Errorf("queryAwsEc2InstanceStates unable to update db with state %s", err.Error())
				return err
			}
		}
	}
	return nil
}

func QueryAzureVMInstanceStates(ctx context.Context, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) error {
	logrus.Infof("processing azure-vm instances due for status check...")

	// get all azure-vm manager ids
	mgrIds, err := db.GetAllManagersByType("azure-vm")
	if err != nil {
		logrus.Errorf("queryAzureVMInstanceStates unable to get node manager ids %s", err.Error())
		return err
	}
	if len(mgrIds) == 0 {
		logrus.Info("no azure-vm managers found")
	}

	// GetStateInfoByManager will get manager connection for each manager
	// and call queryStatus for each region for each manager
	for _, mgrId := range mgrIds {
		instanceStatesResp, err := managers.GetStateInfoByManager(ctx, mgrId, db, "azure-vm", secretsClient)
		if err != nil {
			logrus.Errorf("queryAzureVMInstanceStates unable to get state information for instances %s", err.Error())
			return err
		}
		// get the account id, a unique identifier
		nodeManager, err := db.GetNodeManager(mgrId)
		if err != nil {
			logrus.Errorf("queryAzureVMInstanceStates unable to retrieve manager %s", err.Error())
			return err
		}
		// update each instance's source_state; if source_state != running, update status=unreachable
		for _, inst := range instanceStatesResp {
			_, err := db.UpdateOrInsertInstanceSourceStateInDb(inst, mgrId, nodeManager.AccountId, "azure-vm")
			if err != nil {
				logrus.Errorf("queryAzureVMInstanceStates unable to update db with state %s", err.Error())
				return err
			}
		}
	}
	return nil
}

func fireEvent(eventType string, instanceID string, eventsClient aEvent.EventServiceClient) {
	event := newEventMsg(eventType, instanceID)

	req := aEvent.PublishRequest{Msg: event}
	_, err := eventsClient.Publish(context.Background(), &req)
	if err != nil {
		logrus.Warnf("Error publishing profiles event: %v", err)
		return
	}
}

func newEventMsg(eventType string, instanceID string) *aEvent.EventMsg {

	tagsVal := []string{"node", "nodemanager", "terminate", "compliance", event.NodeTerminatedEventName}

	return &aEvent.EventMsg{
		EventId: uuid.Must(uuid.NewV4()).String(),
		Type:    &aEvent.EventType{Name: eventType},
		Producer: &aEvent.Producer{
			Id:           "urn:chef:compliance:mgrpolling",
			ProducerName: "Node Manager Polling",
			ProducerType: "system component",
		},
		Tags:      tagsVal,
		Published: ptypes.TimestampNow(),
		Actor: &aEvent.Actor{
			Id:          "",
			ObjectType:  "nodemanager",
			DisplayName: "nodemanager",
		},
		Verb: "terminate",
		Object: &aEvent.Object{
			Id:          instanceID,
			ObjectType:  "instance ID",
			DisplayName: instanceID,
		},
		Target: &aEvent.Target{
			Id:          "",
			ObjectType:  "Not Applicable",
			DisplayName: "Not Applicable",
		},
	}
}
