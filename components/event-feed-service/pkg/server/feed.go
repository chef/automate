package server

import (
	"time"

	automate_event "github.com/chef/automate/api/interservice/event"
	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"

	api "github.com/chef/automate/api/interservice/event"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/errors"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/util"
)

type Feeds struct {
	store persistence.FeedStore
}

func NewFeeds(feedStore persistence.FeedStore) *Feeds {
	return &Feeds{store: feedStore}
}

func (f *Feeds) GetFeed(req *event_feed.FeedRequest) ([]*util.FeedEntry, int64, error) {
	// Date Range
	startTime, endTime, err := util.ValidateMillisecondDateRange(req.Start, req.End)
	if err != nil {
		return nil, 0, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// Paging Parameters
	cursorTime, ascending, err := util.ValidatePagingCursorTime(req.Before, req.After, req.Cursor, req.End)
	if err != nil {
		return nil, 0, errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	if req.Size <= 0 {
		return nil, 0, errors.GrpcError(codes.InvalidArgument,
			"The page size must be greater than 0")
	}

	fq := util.FeedQuery{
		UserID:     req.UserID,
		Size:       int(req.Size),
		Start:      startTime.UTC(),
		End:        endTime.UTC(),
		Filters:    req.Filters,
		CursorDate: cursorTime.UTC(),
		CursorID:   req.Cursor,
		Ascending:  ascending,
	}

	resp, hits, err := f.store.GetFeed(&fq)
	if err != nil {
		return nil, 0, errors.GrpcErrorFromErr(codes.Internal, err)
	}
	return resp, hits, nil
}

func (f *Feeds) GetFeedSummary(countCategory string, filters []string, start time.Time, end time.Time) (*util.FeedSummary, error) {

	// remove count category from the filters array
	filters = util.Remove(filters, countCategory)

	fq := util.FeedSummaryQuery{
		CountsCategory: countCategory,
		Buckets:        false,
		Start:          start,
		End:            end,
		Filters:        filters,
	}
	counts, err := f.store.GetFeedSummary(&fq)
	if err != nil {
		logrus.Warnf("Could not get feed summary; error: %+v", err)
		return nil, err
	}

	return &util.FeedSummary{Counts: counts}, nil
}

// interval = time span for which to collect activity entries in hours
func (f *Feeds) GetActionLine(filters []string, startDate string, endDate string, timezone string, interval int, action string) (*util.ActionLine, error) {

	// remove action (aka "task") from the filters array...
	// we're already filtering lines by action
	filters = util.Remove(filters, action)
	line, err := f.store.GetActionLine(filters, startDate, endDate, timezone, interval, action)
	if err != nil {
		logrus.Warnf("Could not get feed timeline; error: %+v", err)
		return &util.ActionLine{}, err
	}

	return line, nil
}

// Going forward, we'll need to maintain a map of event types to feed type(s). Since the
// only events we support currently map to the "event" feed type, we're hard-coding the feed type below.
func (f *Feeds) CreateFeedEntry(req *api.EventMsg) error {
	logrus.Debug("automate-feed is handling your event...")

	publishedAt, err := ptypes.Timestamp(req.Published)
	if err != nil {
		return errors.GrpcErrorFromErr(codes.InvalidArgument, err)
	}

	// translate event message into feed entry
	feedEntry := util.FeedEntry{
		ID:                 uuid.Must(uuid.NewV4()).String(),
		ProducerID:         req.Producer.ID,
		ProducerName:       req.Producer.ProducerName,
		ProducerObjectType: req.Producer.ProducerType,
		ProducerTags:       req.Producer.Tags,
		FeedType:           "event",
		EventType:          req.Type.Name,
		Tags:               req.Tags,
		Published:          publishedAt.UTC(),
		ActorID:            req.Actor.ID,
		ActorName:          req.Actor.DisplayName,
		ActorObjectType:    req.Actor.ObjectType,
		Verb:               req.Verb,
		ObjectID:           req.Object.ID,
		ObjectName:         req.Object.DisplayName,
		ObjectObjectType:   req.Object.ObjectType,
		TargetID:           req.Target.ID,
		TargetName:         req.Target.DisplayName,
		TargetObjectType:   req.Target.ObjectType,
		Created:            time.Now().UTC(),
		OrganizationName:   getOrganizationName(req),
		ChefServerFQDN:     getChefServerFQDN(req),
		ProjectFilterable:  getProjectFilterable(req),
		Projects:           getProjects(req),
	}

	err = f.store.CreateFeedEntry(&feedEntry)
	if err != nil {
		logrus.Warn("Event was not handled... error creating feed entry")
		return err
	}

	logrus.Debug("automate-feed has finished handling your event...")

	return nil
}

func getOrganizationName(event *automate_event.EventMsg) string {
	OrganizationNameTag := "organization_name"
	if event.Data != nil && event.Data.Fields != nil &&
		event.Data.Fields[OrganizationNameTag] != nil &&
		event.Data.Fields[OrganizationNameTag].GetStringValue() != "" {
		return event.Data.Fields[OrganizationNameTag].GetStringValue()
	}

	return ""
}

func getChefServerFQDN(event *automate_event.EventMsg) string {
	ChefServerFQDNTag := "chef_server_fqdn"
	if event.Data != nil && event.Data.Fields != nil &&
		event.Data.Fields[ChefServerFQDNTag] != nil &&
		event.Data.Fields[ChefServerFQDNTag].GetStringValue() != "" {
		return event.Data.Fields[ChefServerFQDNTag].GetStringValue()
	}

	return ""
}

func getProjects(event *automate_event.EventMsg) []string {
	ProjectsTag := "projects"
	if event.Data != nil && event.Data.Fields != nil &&
		event.Data.Fields[ProjectsTag] != nil &&
		event.Data.Fields[ProjectsTag].GetListValue() != nil {
		list := event.Data.Fields[ProjectsTag].GetListValue()

		projects := make([]string, 0)
		for _, listItem := range list.GetValues() {
			if listItem.GetStringValue() != "" {
				projects = append(projects, listItem.GetStringValue())
			}
		}

		return projects
	}

	return []string{}
}

func getProjectFilterable(event *automate_event.EventMsg) bool {
	ProjectFilterableTag := "project_filterable"
	if event.Data != nil && event.Data.Fields != nil &&
		event.Data.Fields[ProjectFilterableTag] != nil {
		return event.Data.Fields[ProjectFilterableTag].GetBoolValue()
	}

	// Default to filtering with projects
	return true
}
