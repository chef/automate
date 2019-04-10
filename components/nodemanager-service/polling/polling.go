package polling

import (
	"context"
	"strings"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/api/external/secrets"
	aEvent "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/event-service/server"

	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/config"
	"github.com/chef/automate/components/nodemanager-service/managers"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
)

func StartThePolls(ctx context.Context, mgrConf config.Manager, db *pgdb.DB, secretsClient secrets.SecretsServiceClient, eventsClient aEvent.EventServiceClient) {
	go PollAwsEc2InstanceState(ctx, mgrConf.AwsEc2PollIntervalMinutes, db, secretsClient, eventsClient)
	go PollAzureVMInstanceState(ctx, mgrConf.AzureVMPollIntervalMinutes, db, secretsClient)
	go PollManagersStatus(ctx, db, secretsClient)
}

func PollManagersStatus(ctx context.Context, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) {
	for {
		time.Sleep(120 * time.Minute)
		go checkManagersStatuses(ctx, db, secretsClient)
	}
}

func checkManagersStatuses(ctx context.Context, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) {
	// get all managers
	// note: we really don't expect anyone to have more than 100 nodemanagers, so this is ok for now
	allManagers, _, err := db.GetNodeManagers("", 0, 1, 100, []*common.Filter{})
	if err != nil {
		logrus.Errorf("checkManagersStatuses unable to check for node managers %s", err.Error())
		return
	}
	// do a dry run connection on all aws and azure managers to ensure creds are still good
	for _, mgr := range allManagers {
		if strings.HasPrefix(mgr.Type, "aws") {
			myaws, err := managers.GetAWSManagerFromCredential(ctx, mgr.CredentialId, db, secretsClient)
			if err != nil {
				logrus.Error(err)
				return
			}
			err = myaws.TestConnectivity(ctx)
			handleManagerStatusError(db, mgr, err)
		}
		if strings.HasPrefix(mgr.Type, "azure") {
			myazure, err := managers.GetAzureManagerFromCredential(ctx, mgr.CredentialId, db, secretsClient)
			if err != nil {
				logrus.Error(err)
				return
			}
			err = myazure.TestConnectivity(ctx)
			handleManagerStatusError(db, mgr, err)
		}
	}
}

func handleManagerStatusError(db *pgdb.DB, mgr *manager.NodeManager, err error) {
	// if they are not successful, update manager status to be "unreachable"
	if err != nil {
		logrus.Errorf("updating manager %s with status unreachable due to error: %s", mgr.Name, err.Error())
		err := db.UpdateManagerStatus(mgr.Id, "unreachable")
		if err != nil {
			logrus.Errorf("unable to update manager status: %s", err.Error())
		}
	} else {
		logrus.Infof("Connection check successful for manager: %s", mgr.Name)
	}
}

func PollAwsEc2InstanceState(ctx context.Context, pollInterval int, db *pgdb.DB, secretsClient secrets.SecretsServiceClient, eventsClient aEvent.EventServiceClient) {
	interval := time.Minute * time.Duration(pollInterval)
	for {
		time.Sleep(interval)
		go queryAwsEc2InstanceStates(ctx, db, interval, secretsClient, eventsClient)
	}
}

func queryAwsEc2InstanceStates(ctx context.Context, db *pgdb.DB, interval time.Duration, secretsClient secrets.SecretsServiceClient, eventsClient aEvent.EventServiceClient) {
	logrus.Infof("processing aws-ec2 instances due for status check...")

	// get all aws-ec2 manager ids
	mgrIds, err := db.GetAllManagersByType("aws-ec2")
	if err != nil {
		logrus.Errorf("queryAwsEc2InstanceStates unable to get node manager ids %s", err.Error())
		return
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
			return
		}
		// get the account id, a unique identifier
		nodeManager, err := db.GetNodeManager(mgrId)
		if err != nil {
			logrus.Errorf("queryAwsEc2InstanceStates unable to retrieve manager %s", err.Error())
			return
		}
		// update each instance's source_state; if source_state != running, update status=unreachable
		for _, inst := range instanceStatesResp {

			// We use the updated boolean to determine if a node enters a new state
			// this prevents us from sending out events continuously about already terminated nodes.
			updated, err := db.UpdateOrInsertInstanceSourceStateInDb(inst, mgrId, nodeManager.AccountId, "aws-ec2")
			if inst.State == "terminated" && updated {
				fireEvent(server.NodeTerminated, inst.ID, eventsClient)
			}
			if err != nil {
				logrus.Errorf("queryAwsEc2InstanceStates unable to update db with state %s", err.Error())
				return
			}
		}
	}
}

func PollAzureVMInstanceState(ctx context.Context, pollInterval int, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) {
	interval := time.Minute * time.Duration(pollInterval)
	for {
		time.Sleep(interval)
		go queryAzureVMInstanceStates(ctx, db, interval, secretsClient)
	}
}

func queryAzureVMInstanceStates(ctx context.Context, db *pgdb.DB, interval time.Duration, secretsClient secrets.SecretsServiceClient) {
	logrus.Infof("processing azure-vm instances due for status check...")

	// get all azure-vm manager ids
	mgrIds, err := db.GetAllManagersByType("azure-vm")
	if err != nil {
		logrus.Errorf("queryAzureVMInstanceStates unable to get node manager ids %s", err.Error())
		return
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
			return
		}
		// get the account id, a unique identifier
		nodeManager, err := db.GetNodeManager(mgrId)
		if err != nil {
			logrus.Errorf("queryAzureVMInstanceStates unable to retrieve manager %s", err.Error())
			return
		}
		// update each instance's source_state; if source_state != running, update status=unreachable
		for _, inst := range instanceStatesResp {
			_, err := db.UpdateOrInsertInstanceSourceStateInDb(inst, mgrId, nodeManager.AccountId, "azure-vm")
			if err != nil {
				logrus.Errorf("queryAzureVMInstanceStates unable to update db with state %s", err.Error())
				return
			}
		}
	}
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

	tagsVal := []string{"node", "nodemanager", "terminate", "compliance", server.NodeTerminated}

	return &aEvent.EventMsg{
		EventID: uuid.Must(uuid.NewV4()).String(),
		Type:    &aEvent.EventType{Name: eventType},
		Producer: &aEvent.Producer{
			ID:           "urn:chef:compliance:mgrpolling",
			ProducerName: "Node Manager Polling",
			ProducerType: "system component",
		},
		Tags:      tagsVal,
		Published: ptypes.TimestampNow(),
		Actor: &aEvent.Actor{
			ID:          "",
			ObjectType:  "nodemanager",
			DisplayName: "nodemanager",
		},
		Verb: "terminate",
		Object: &aEvent.Object{
			ID:          instanceID,
			ObjectType:  "instance ID",
			DisplayName: instanceID,
		},
		Target: &aEvent.Target{
			ID:          "",
			ObjectType:  "Not Applicable",
			DisplayName: "Not Applicable",
		},
	}
}
