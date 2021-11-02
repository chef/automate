package grpcserver

import (
	"context"
	"encoding/json"
	"time"

	"github.com/chef/automate/components/config-mgmt-service/backend"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/golang/protobuf/proto"
	"github.com/golang/protobuf/ptypes"
	gp "github.com/golang/protobuf/ptypes/struct"
	google_protobuf1 "github.com/golang/protobuf/ptypes/timestamp"
	"github.com/sirupsen/logrus"
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

	filters[backend.Project] = projectsFilter
	return filters, nil
}

// DaysBetween get the calendar days between two timestamp
func DaysBetween(fromTime, toTime time.Time) int {
	if fromTime.After(toTime) {
		fromTime, toTime = toTime, fromTime
	}

	days := -fromTime.YearDay()
	for year := fromTime.Year(); year < toTime.Year(); year++ {
		days += time.Date(year, time.December, 31, 0, 0, 0, 0, time.UTC).YearDay()
	}
	days += toTime.YearDay()

	return days
}

func LogQueryPartMin(indices string, partToPrint interface{}, name string) {
	part, err := json.Marshal(partToPrint)
	if err != nil {
		logrus.Errorf("%s", err)
	}
	stringPart := string(part)
	if stringPart == "null" {
		stringPart = ""
	} else {
		stringPart = "\n" + stringPart
	}
	logrus.Debugf("\n------------------ %s-(start)--[%s]---------------%s \n------------------ %s-(end)-----------------------------------\n",
		name, indices, stringPart, name)
}
