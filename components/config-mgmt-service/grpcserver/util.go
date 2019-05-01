package grpcserver

import (
	"context"
	"time"

	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes"
	gp "github.com/golang/protobuf/ptypes/struct"
	google_protobuf1 "github.com/golang/protobuf/ptypes/timestamp"
)

// messageArrayToListValue Casts a 'Proto Message Array' into a 'Proto ListValue'
func messageArrayToListValue(messages []proto.Message, list *gp.ListValue) error {
	list.Values = make([]*gp.Value, len(messages))

	for i, msg := range messages {
		v := gp.Value{}
		content, err := getMessageRawJSON(msg)
		if err != nil {
			return err
		}

		err = protoFromJSON(content, &v)
		if err != nil {
			return err
		}

		list.Values[i] = &v
	}
	return nil
}

func ToTime(timestamp *google_protobuf1.Timestamp) (time.Time, error) {
	if timestamp == nil {
		return time.Time{}, nil
	}
	return ptypes.Timestamp(timestamp)
}

func filterByProjects(ctx context.Context, filters map[string][]string) (map[string][]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		return filters, nil
	}

	filters["projects"] = projectsFilter
	return filters, nil
}
