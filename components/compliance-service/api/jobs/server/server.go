package server

import (
	"context"
	"fmt"
	"strings"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	pb "github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	rrule "github.com/teambition/rrule-go"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/components/compliance-service/api/jobs"
	"github.com/chef/automate/components/compliance-service/dao/pgdb"
	"github.com/chef/automate/components/compliance-service/inspec-agent/scheduler"
	"github.com/chef/automate/components/compliance-service/scanner"
	event "github.com/chef/automate/components/event-service/config"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/errorutils"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/secureconn"
)

// Server implementation for jobs
type Server struct {
	db              *pgdb.DB
	connFactory     *secureconn.Factory
	eventsClient    automate_event.EventServiceClient
	schedulerServer *scheduler.Scheduler
}

var empty = pb.Empty{}

// New creates a new jobs server
func New(db *pgdb.DB, connFactory *secureconn.Factory, eventsClient automate_event.EventServiceClient,
	managerEndpoint string, cerealManager *cereal.Manager) *Server {
	conf := &Server{
		db:           db,
		connFactory:  connFactory,
		eventsClient: eventsClient,
	}
	conf.getComplianceAndSecretsConnection(connFactory, db, managerEndpoint, cerealManager)
	return conf
}

// get the ManagerClient, NodesClient, and IngestClient to be able to set up the scheduler server
// the scheduler server is used to call the inspec-agent
func (srv *Server) getComplianceAndSecretsConnection(
	connectionFactory *secureconn.Factory, db *pgdb.DB,
	managerEndpoint string, cerealManager *cereal.Manager) {
	if managerEndpoint == "" {
		logrus.Errorf("complianceEndpoint and managerEndpoint cannot be empty or Dial will get stuck")
		return
	}
	// dial manager
	mgrConn, err := connectionFactory.Dial("nodemanager-service", managerEndpoint)
	if err != nil {
		logrus.Errorf("getComplianceAndSecretsConnection, error grpc dialing to nodemanager %s", err.Error())
		return
	}
	// get manager client with manager conn
	mgrClient := manager.NewNodeManagerServiceClient(mgrConn)
	if mgrClient == nil {
		logrus.Errorf("getComplianceAndSecretsConnection got nil for NewNodeManagerServiceClient")
		return
	}
	// get node client with manager conn
	nodesClient := nodes.NewNodesServiceClient(mgrConn)
	if nodesClient == nil {
		logrus.Errorf("getComplianceAndSecretsConnection got nil for NewNodesServiceClient")
		return
	}

	scanner := scanner.New(mgrClient, nodesClient, db)
	srv.schedulerServer = scheduler.New(scanner, cerealManager)
}

// GetJobResultByNodeId returns the results row for a given job id and node id
func (srv *Server) GetJobResultByNodeId(ctx context.Context, in *jobs.GetJobResultByNodeIdRequest) (*jobs.ResultsRow, error) {
	if len(in.NodeId) == 0 || len(in.JobId) == 0 {
		return nil, errorutils.ProcessInvalid(nil, "Invalid request: node id and job id are required fields")
	}
	return srv.db.GetJobResultByNodeId(ctx, in)
}

// Create creates a new job
func (srv *Server) Create(ctx context.Context, in *jobs.Job) (*jobs.Id, error) {
	logrus.Debugf("Create a new job: %+v", in)
	if in.Recurrence != "" {
		// Ensure recurrence rule can be parsed
		_, err := rrule.StrToRRule(in.Recurrence)
		if err != nil {
			err = errorutils.ProcessInvalid(nil, fmt.Sprintf("Invalid job recurrence rule %+v", err))
			return nil, errorutils.FormatErrorMsg(err, "")
		}
	}

	if in.NodeSelectors == nil && in.Nodes == nil {
		return nil, status.Error(codes.InvalidArgument, "Invalid job: nodes or node selectors required.")
	}

	sID, err := srv.db.AddJob(in)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	in.Id = sID
	// Trigger Agent. Agents are responsible for all further steps.
	go srv.schedulerServer.Run(in) // nolint: errcheck

	if in.Type == "exec" {
		// fire scan job created event
		user := getUserValFromCtx(ctx)
		go srv.fireEvent(event.ScanJobCreatedEventName, in, nil, user)
	}
	return &jobs.Id{Id: sID, Name: in.Name}, nil
}

// Read a job via ID
func (srv *Server) Read(ctx context.Context, in *jobs.Id) (*jobs.Job, error) {
	logrus.Debugf("Read job with id: %+v", in.Id)
	node, err := srv.db.GetJob(in.Id)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	return node, nil
}

// Update one job
func (srv *Server) Update(ctx context.Context, in *jobs.Job) (*pb.Empty, error) {
	logrus.Debugf("Update job with id: %+v", in)
	if in.Recurrence == "" && in.ParentId != "" {
		err := &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid job. Child jobs may not be updated. If you wish to update the parent job, please find job: " + in.ParentId)}
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	if in.Recurrence != "" {
		// Ensure recurrence rule can be parsed
		_, err := rrule.StrToRRule(in.Recurrence)
		if err != nil {
			err := &errorutils.InvalidError{Msg: fmt.Sprintf("Invalid job recurrence rule %+v", in.Recurrence)}
			return nil, errorutils.FormatErrorMsg(err, in.Id)
		}
	}

	err := srv.db.UpdateJob(in)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	// Trigger Agent. Agents are responsible for all further steps.
	go srv.schedulerServer.Run(in) // nolint: errcheck

	// fire scan job updated event
	user := getUserValFromCtx(ctx)
	go srv.fireEvent(event.ScanJobUpdatedEventName, in, nil, user)

	return &empty, nil
}

// Delete a job
func (srv *Server) Delete(ctx context.Context, in *jobs.Id) (*pb.Empty, error) {
	logrus.Debugf("Deleting job id: %+v", in.Id)

	// delete the job (this is a soft delete)
	err := srv.db.DeleteJob(in.Id)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	// get job name to populate event feed with useful info later
	name, err := srv.db.GetJobName(in.GetId())
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	// fire scan job deleted event
	user := getUserValFromCtx(ctx)
	job := jobs.Job{Name: name}
	go srv.fireEvent(event.ScanJobDeletedEventName, &job, in, user)

	return &empty, nil
}

// given a ctx, we inspect and take the auth subject user
// this is used to populate information for the event feed
func getUserValFromCtx(ctx context.Context) (user string) {
	aCtx := auth_context.FromContext(auth_context.FromIncomingMetadata(ctx))
	logrus.Debugf("auth context %+v", aCtx)
	if aCtx != nil {
		for _, sub := range aCtx.Subjects {
			if strings.HasPrefix(sub, "user:") {
				// expected format is "user:local:alice"
				fullUserDetails := strings.Split(sub, ":")
				// we only care about the username here, we don't need
				// to distinguish between saml/local/ldap
				user = fullUserDetails[len(fullUserDetails)-1]
				break
			}
		}
	}
	return
}

// List jobs based on a query
func (srv *Server) List(ctx context.Context, in *jobs.Query) (*jobs.Jobs, error) {
	logrus.Debugf("Getting Jobs with query: %+v", in)
	dbjobs, totalCount, err := srv.db.GetJobs(in.Sort, in.Order, in.Page, in.PerPage, in.Filters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	return &jobs.Jobs{Jobs: dbjobs, Total: int32(totalCount)}, nil
}

// Rerun a job. Does not create a new job in database. Reads the job info from db
// given the job id, and sends that information over to the scheduler
func (srv *Server) Rerun(ctx context.Context, in *jobs.Id) (*jobs.RerunResponse, error) {
	logrus.Debugf("Rerunning job with id: %s", in.Id)
	job, err := srv.db.GetJob(in.Id)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	if job.Recurrence != "" || job.ParentId != "" {
		return nil, status.Error(codes.InvalidArgument, "Unable to rerun a scheduled job")
	}
	// Trigger Agent. Agents are responsible for all further steps.
	go srv.schedulerServer.Run(job) // nolint: errcheck

	return &jobs.RerunResponse{}, nil
}

// ListInitiatedScans returns a list of ids for all scans with an end_time after or equal to the time
// provided or status 'running'. This is used for scan jobs auditing purposes.
func (srv *Server) ListInitiatedScans(ctx context.Context, in *jobs.TimeQuery) (*jobs.Ids, error) {
	jobIDList, err := srv.db.ListInitiatedScans(ctx, in.StartTime)
	if err != nil {
		return nil, errors.Wrap(err, "unable to list owca scans")
	}
	return &jobs.Ids{Ids: jobIDList}, nil
}

// hand over the event to the event-service
func (srv *Server) fireEvent(eventType string, in *jobs.Job, id *jobs.Id, user string) {
	event := srv.newEventMsg(eventType, in, id, user)
	req := automate_event.PublishRequest{Msg: event}
	_, err := srv.eventsClient.Publish(context.Background(), &req)
	if err != nil {
		logrus.Warnf("Error publishing scan job event: %v", err)
		return
	}
}

func (srv *Server) newEventMsg(eventType string, in *jobs.Job, id *jobs.Id, user string) *automate_event.EventMsg {
	var (
		verbVal string
		tagsVal []string
		nameVal string
		userVal string
	)

	if len(user) == 0 {
		user = "UI User"
	}

	switch eventType {

	case event.ScanJobCreatedEventName:
		userVal = user
		verbVal = "create"
		tagsVal = []string{"scanjobs", user, "create", "compliance", event.ScanJobCreatedEventName}
		nameVal = in.Name

	case event.ScanJobUpdatedEventName:
		userVal = user
		verbVal = "update"
		tagsVal = []string{"scanjobs", user, "update", "compliance", event.ScanJobUpdatedEventName}
		nameVal = in.Name

	case event.ScanJobDeletedEventName:
		userVal = user
		verbVal = "delete"
		tagsVal = []string{"scanjobs", user, "delete", "compliance", event.ScanJobDeletedEventName}
		nameVal = in.Name

	default:
		logrus.Warnf("Could not generate event message for scan job %s; unknown event type %s", in.Name, eventType)
	}

	return &automate_event.EventMsg{
		EventID: uuid.Must(uuid.NewV4()).String(),
		Type:    &automate_event.EventType{Name: eventType},
		Producer: &automate_event.Producer{
			ID:           "scanjobs",
			ProducerName: "Scanner",
			ProducerType: "system component",
		},
		Tags:      tagsVal,
		Published: ptypes.TimestampNow(),
		Actor: &automate_event.Actor{
			ID:          "",
			ObjectType:  "User",
			DisplayName: userVal,
		},
		Verb: verbVal,
		Object: &automate_event.Object{
			ID:          uuid.Must(uuid.NewV4()).String(),
			ObjectType:  "scanjobs",
			DisplayName: nameVal,
		},
		Target: &automate_event.Target{
			ID:          "",
			ObjectType:  "Not Applicable",
			DisplayName: "Not Applicable",
		},
	}
}
