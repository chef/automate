package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/feed"
	"github.com/chef/automate/lib/io/chunks"
	"github.com/gocarina/gocsv"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// Chosen somewhat arbitrarily to be a "good enough" value.
// See: https://github.com/chef/automate/pull/1143#discussion_r170428374
const streamBufferSize = 262144

type exportHandler func([]*feed.FeedEntry) error

type displayEvent struct {
	EventType        string    `csv:"Event Type" json:"event_type"`
	ID               string    `csv:"ID" json:"id"`
	Task             string    `csv:"Task" json:"task"`
	RequestorName    string    `csv:"Requestor Name" json:"requestor_name"`
	RequestorType    string    `csv:"Requestor Type" json:"requestor_type"`
	ChefOrganization string    `csv:"Chef Organization" json:"chef_organization"`
	ChefInfraServer  string    `csv:"Chef Infra Server" json:"chef_infra_server"`
	ParentName       string    `csv:"Parent Name" json:"parent_name"`
	ParentID         string    `csv:"Parent ID" json:"parent_id"`
	Date             time.Time `csv:"Date" json:"date"`
}

// EventExport downloading events report.
func (eventFeedServer *EventFeedServer) EventExport(request *event_feed.EventExportRequest,
	stream event_feed.EventFeedService_EventExportServer) error {
	var sendResult exportHandler

	// Date Range
	startTime, endTime, err := feed.ValidateMillisecondDateRange(request.Start, request.End)
	if err != nil {
		return status.Error(codes.InvalidArgument, err.Error())
	}

	switch request.OutputType {
	case "", "json":
		sendResult = jsonExport(stream)
	case "csv":
		sendResult = csvExport(stream)
	default:
		return status.Error(codes.Unauthenticated, fmt.Sprintf("%s export is not supported", request.OutputType))
	}

	streamCtx := stream.Context()
	deadline, ok := streamCtx.Deadline()
	if !ok {
		deadline = time.Now().Add(5 * time.Minute)
	}
	ctx, cancel := context.WithDeadline(streamCtx, deadline)
	defer cancel()

	filters, err := feed.FormatFilters(request.Filter)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, err.Error())
	}

	filters, err = filterByProjects(ctx, filters)
	if err != nil {
		return status.Errorf(codes.InvalidArgument, err.Error())
	}

	sortAsc := request.Order.String() != "desc"

	entryPager := eventFeedServer.entryPager(ctx, sortAsc, filters, startTime, endTime)

	for {
		entries, err := entryPager()
		if err != nil {
			return status.Errorf(codes.Internal, err.Error())
		}

		err = sendResult(entries)
		if err != nil {
			return status.Errorf(codes.Internal, "Failed to stream events Error: %s", err.Error())
		}

		if len(entries) == 0 {
			break
		}
	}

	return nil
}

func entryToDisplayEvent(entry *feed.FeedEntry) displayEvent {
	return displayEvent{
		ID:               entry.ID,
		EventType:        entry.ObjectName,
		Task:             entry.Verb,
		RequestorName:    entry.ActorName,
		RequestorType:    entry.ActorObjectType,
		ChefOrganization: entry.ChefOrganization,
		ChefInfraServer:  entry.ChefInfraServer,
		ParentName:       entry.ParentName,
		ParentID:         entry.ParentID,
		Date:             entry.Published,
	}
}

func jsonExport(stream event_feed.EventFeedService_EventExportServer) exportHandler {
	initialRun := true
	return func(entries []*feed.FeedEntry) error {
		// If this is the first set of nodes to export, prepend "[" to start a JSON document.
		if initialRun {
			err := stream.Send(&event_feed.EventExportResponse{Content: []byte{'['}})
			if err != nil {
				return err
			}
			initialRun = false
			// If this is not the first set of nodes and the collection has elements,
			// prepend the "," that couldn't get added in the previous call to this function.
		} else if len(entries) != 0 {
			err := stream.Send(&event_feed.EventExportResponse{Content: []byte{','}})
			if err != nil {
				return err
			}
		}

		// If the collection has no elements, append "]" to close the JSON document and stop.
		if len(entries) == 0 {
			err := stream.Send(&event_feed.EventExportResponse{Content: []byte{']'}})
			if err != nil {
				return err
			}
			return nil
		}

		writer := chunks.NewWriter(streamBufferSize, func(p []byte) error {
			return stream.Send(&event_feed.EventExportResponse{Content: p})
		})
		buf := make([]byte, streamBufferSize)

		for i := range entries {
			raw, err := json.Marshal(entryToDisplayEvent(entries[i]))
			if err != nil {
				return fmt.Errorf("Failed to marshal JSON export data: %+v", err)
			}

			// If this is the last element of the collection passed to this function call,
			// there is not enough information to know if this is the last element overall.
			// So, only append "," if it can be determined that there are more elements.
			if i != len(entries)-1 {
				raw = append(raw, ',')
			}

			reader := bytes.NewReader(raw)

			_, err = io.CopyBuffer(writer, reader, buf)
			if err != nil {
				return fmt.Errorf("Failed to export JSON: %+v", err)
			}
		}

		return nil
	}
}

func csvExport(stream event_feed.EventFeedService_EventExportServer) exportHandler {
	initialRun := true
	return func(entries []*feed.FeedEntry) error {
		res, err := entriesToCSV(entries)
		if err != nil {
			return err
		}

		if initialRun {
			initialRun = false
		} else {
			splits := strings.SplitAfterN(res, "\n", 2)
			if len(splits) == 2 {
				res = splits[1]
			}
		}

		reader := bytes.NewReader([]byte(res))
		buf := make([]byte, streamBufferSize)

		writer := chunks.NewWriter(streamBufferSize, func(p []byte) error {
			return stream.Send(&event_feed.EventExportResponse{Content: p})
		})
		_, err = io.CopyBuffer(writer, reader, buf)
		if err != nil {
			return fmt.Errorf("Failed to export CSV: %+v", err)
		}

		return nil
	}
}

func entriesToCSV(entries []*feed.FeedEntry) (string, error) {
	displayNodeCollection := nodeCollectionToDisplayEventCollection(entries)

	// export everything
	content, err := gocsv.MarshalString(&displayNodeCollection)
	if err != nil {
		return "", fmt.Errorf("Failed to marshal CSV report: %+v", err)
	}
	return content, nil
}

func nodeCollectionToDisplayEventCollection(entries []*feed.FeedEntry) []displayEvent {
	var displayEntryCollection = make([]displayEvent, len(entries))
	for index, entry := range entries {
		displayEntryCollection[index] = entryToDisplayEvent(entry)
	}

	return displayEntryCollection
}

func (eventFeedServer *EventFeedServer) entryPager(ctx context.Context,
	sortAsc bool, filters map[string][]string, startTime, endTime time.Time) func() ([]*feed.FeedEntry, error) {
	pageSize := 100
	var cursorDate time.Time
	cursorID := ""

	return func() ([]*feed.FeedEntry, error) {
		fq := feed.FeedQuery{
			Size:       pageSize,
			Start:      startTime.UTC(),
			End:        endTime.UTC(),
			Filters:    filters,
			CursorDate: cursorDate.UTC(),
			CursorID:   cursorID,
			Ascending:  sortAsc,
		}

		entries, _, err := eventFeedServer.feedService.store.GetFeed(&fq)
		if err != nil {
			return []*feed.FeedEntry{}, err
		}

		if len(entries) == 0 {
			return []*feed.FeedEntry{}, nil
		}

		lastEntry := entries[len(entries)-1]
		cursorID = lastEntry.ID
		cursorDate = lastEntry.Published

		return entries, nil
	}
}
