package elastic

import (
	"context"
	"fmt"
	"strings"
	"time"

	"github.com/olivere/elastic"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
)

const (
	testMapping = `{
		"settings": {
			"number_of_shards":1,
			"number_of_replicas":0
		},
		"mappings:": {
			"test-data" : {
				"properties" : {
					"end_time": { "type" : "date" }
				}
			}
		}
	}`
	docDeleteQueryFmt = `{
		"range" : {
			"%s" : {
				"lte" : "now/d-%dd"
			}
		}
	}`
)

// Elastic is the interface to this component.
type Elastic struct {
	client *elastic.Client
}

// HostDiskStats captures a collection of PerDiskStats
// for a given ES node.
type HostDiskStats struct {
	Host         string
	PerDiskStats []DiskStats
}

// DiskStats captures the disk usage stats for each
// data path on an ES cluster node, as reported by ES
type DiskStats struct {
	MountPoint     string
	TotalBytes     int64
	AvailableBytes int64
}

// New connects to the provided ES server instance and returns an Elastic instance
// containing a client
func New(esURL string) (*Elastic, error) {
	client, err := elastic.NewClient(
		// NOTE - take a look at docs here to see what's relevant:
		// https://github.com/olivere/elastic/wiki/Configuration
		elastic.SetURL(esURL),
		elastic.SetSniff(false),
	)

	if err != nil {
		return nil, errors.Wrap(err, "Failed to create ES client")
	}

	return &Elastic{client: client}, nil
}

// DeleteTimeSeriesIndicesByAge deletes all indices names as baseName-YYYY-mm-dd that are older than
// olderThanDays. It looks only at dates in the index name when determining age - it does not consider time
// or reference any dates in the individual documents contained by the indices.
func (es *Elastic) DeleteTimeSeriesIndicesByAge(ctx context.Context, baseName string, olderThanDays int) error {
	start := time.Now()
	deleted := 0
	indices, err := es.DailyIndicesFor(ctx, baseName)

	if err != nil {
		log.WithError(err).WithFields(log.Fields{
			"baseName": baseName,
		}).Debug("Error: Could not list indices")
		return err
	}

	prefix := baseName + "-"
	olderThanDate := start.AddDate(0, 0, -olderThanDays)
	for _, indexName := range indices {
		if strings.HasPrefix(indexName, prefix) {
			trimmed := strings.TrimPrefix(indexName, prefix)

			t, err := time.Parse("2006.01.02", trimmed)
			if err != nil {
				log.WithError(err).Warn("Could not parse index date")
				continue
			}
			if t.Before(olderThanDate) {
				deleteResult, err := es.client.DeleteIndex(indexName).Do(ctx)
				if err == nil {
					deleted++
					log.WithFields(log.Fields{
						"index": indexName,
						"ack":   deleteResult.Acknowledged,
					}).Debug("Index deleted")
				} else {
					if elastic.IsNotFound(err) {
						log.WithError(err).WithField("index", indexName).Warn("Could not delete index")
					} else {
						log.WithError(err).WithField("index", indexName).Errorf("Could not delete index")
						return err
					}
				}
			}
		} else {
			// We should never get here
			log.WithFields(log.Fields{
				"baseName": baseName,
				"index":    indexName,
			}).Debug("Invalid index name found")
			continue
		}
	}

	if deleted == 0 {
		log.WithField("olderThan", olderThanDate).Debug("No indices deleted")
	}

	return nil
}

// FailedDeletion represents a document that could not be deleted.
type FailedDeletion struct {
	ID   string
	Type string
}

// DeleteError occurs when DeleteDocumentsFromIndexByAge is able to
// successfully invoke the ES DeleteByQuery API and the request completes without
// error, but reports that it was not able to delete the requested documents.
type DeleteError struct {
	Failures []FailedDeletion
}

func (f DeleteError) Error() string {
	// failure details aren't exposed in the message, but they
	// are available in the returned error for the caller to make use of.
	return "Delete request aborted."
}

// NotFoundError occurs when ES reports the requested index to be not found.
type NotFoundError struct {
}

func (f NotFoundError) Error() string {
	return "Index not found"
}

// DeleteDocumentsFromIndexByAge deletes all documents in the specified index that are
// older than olderThanDays.  Unlike DeleteTimeSeriesIndicesByAge, it looks at the complete
// date-time in determining age.
// It expects that the indexed contains a mapping of field 'end_time' to date.
func (es *Elastic) DeleteDocumentsFromIndexByAge(ctx context.Context, index string, olderThanDays int, customPurgeField string) error {
	purgeField := "end_time"
	if customPurgeField != "" {
		purgeField = customPurgeField
	}
	queryString := fmt.Sprintf(docDeleteQueryFmt, purgeField, olderThanDays)
	service := es.client.DeleteByQuery(index).
		Refresh("true").
		Query(elastic.RawStringQuery(queryString))
	response, err := service.Do(ctx)

	if err != nil {
		if elastic.IsNotFound(err) {
			return NotFoundError{}
		}
		log.WithFields(log.Fields{
			"index":           index,
			"older-than-days": olderThanDays,
			"purge-field":     purgeField,
		}).WithError(err).Debug("document delete failed")
		return errors.Wrap(err, "An error occurred while deleting documents from the requested index")
	}
	// per API docs, if 'failures' has any values then the request was aborted.
	// cf https://www.elastic.co/guide/en/elasticsearch/reference/5.6/docs-delete-by-query.html
	if len(response.Failures) > 0 {
		failedDeletions := make([]FailedDeletion, len(response.Failures))
		for i, failure := range response.Failures {
			failedDeletions[i] = FailedDeletion{ID: failure.Id, Type: failure.Type}
		}
		return DeleteError{Failures: failedDeletions}
	}

	log.WithFields(log.Fields{
		"index":           index,
		"older-than-days": olderThanDays,
		"purge-field":     purgeField,
		"num-deleted":     response.Deleted,
	}).Debug("document delete completed")
	return nil
}

// GetDiskStats returns a slice of disk usage detail from all active nodes in the ES cluster.
func (es *Elastic) GetDiskStats(ctx context.Context) ([]HostDiskStats, error) {
	resp, err := es.client.NodesStats().Metric("fs").Do(ctx)

	if err != nil {
		return nil, errors.Wrap(err, "Failed to get node stats")
	}

	hostDiskStatsArr := make([]HostDiskStats, 0, len(resp.Nodes))

	for _, nodeStats := range resp.Nodes {
		perDiskStats := make([]DiskStats, 0, len(nodeStats.FS.Data))
		for _, fs := range nodeStats.FS.Data {
			perDiskStats = append(
				perDiskStats,
				DiskStats{
					MountPoint:     fs.Mount,
					TotalBytes:     fs.TotalInBytes,
					AvailableBytes: fs.AvailableInBytes,
				},
			)
		}
		hostDiskStatsArr = append(
			hostDiskStatsArr,
			HostDiskStats{
				Host:         nodeStats.Host,
				PerDiskStats: perDiskStats,
			},
		)
	}
	return hostDiskStatsArr, nil
}

func (es *Elastic) indexTimeseriesFmt(baseName string, date time.Time) string {
	utcDate := date.UTC().Format("2006.01.02")
	return fmt.Sprintf("%s-%s", baseName, utcDate)
}

// NOTE: The functions below this point are used for simplifying development and are not required for
//       functionality exposed by the es-sidecar-service service itself.

// CreateRepository creates an ES repository for backups in a pre-configured location that's
// compatible with our test es5 setup for this component (reference test/elastic/config/elasticsearch.yml
// for config details, and the 'start-es5' target in es-sidecar-service's Makefile).
// This is a development aid.
func (es *Elastic) CreateRepository(ctx context.Context, name string) (bool, error) {
	service := es.client.SnapshotCreateRepository(name)
	service = service.Type("fs").
		Setting("location", "/usr/share/elasticsearch/data/repo").
		Setting("compress", true)

	// BUG: due to a bug in the elastic.v5 SnapshotCreateRepositoryService
	// that fails this call with a validation error without sending it to ES.
	// The patch that corrects this (not yet submitted upstream at time of writing)
	// is here: https://github.com/olivere/elastic/compare/release-branch.v5...marcparadise:v5-create-repository
	res, err := service.Do(ctx)

	if err != nil {
		log.WithFields(log.Fields{
			"repository": service,
		}).WithError(err).Debug("create repository failed")
		return false, err
	}
	return res.Acknowledged, nil
}

// AllIndices returns an array of all index names.
// This is a development aid.
func (es *Elastic) AllIndices() ([]string, error) {
	names, err := es.client.IndexNames()
	if err != nil {
		return nil, errors.Wrap(err, "ShowIndices failed")
	}
	return names, nil
}

// DailyIndicesFor returns the indices starting with indexName
func (es *Elastic) DailyIndicesFor(ctx context.Context, baseName string) ([]string, error) {
	if baseName == "" {
		return nil, errors.New("Empty index name not allowed")
	}

	res, err := es.client.IndexGetSettings(fmt.Sprintf("%s-*", baseName)).Do(ctx)
	if err != nil {
		return nil, err
	}

	names := []string{}
	for name := range res {
		trimmed := strings.TrimPrefix(name, baseName+"-")

		_, err := time.Parse("2006.01.02", trimmed)
		if err != nil {
			continue
		}

		names = append(names, name)
	}
	return names, nil
}

// CreateTimeNamedIndices creates a series of indices based on the provided
// time and number of days. This is a development aid.
func (es *Elastic) CreateTimeNamedIndices(ctx context.Context, start time.Time, baseName string, days int) {
	for daysBack := 0; daysBack < days; daysBack++ {
		es.createTimeseriesIndex(ctx, start, baseName, daysBack)
	}
}

// MakeMeAnIndex creates a new index with the given name. This is a development and testing aid.
// TODO - this could probable just replace createTimeSeriesIndex below.
func (es *Elastic) MakeMeAnIndex(ctx context.Context, name string) error {

	_, err := es.client.CreateIndex(name).BodyString(testMapping).Do(ctx)
	if err != nil {
		return err
	}
	log.WithFields(log.Fields{"name": name}).Debug("Index created")
	return nil
}

func (es *Elastic) createTimeseriesIndex(ctx context.Context, start time.Time, baseName string, daysBack int) {
	name := es.indexTimeseriesFmt(baseName, start.AddDate(0, 0, -daysBack))
	exists, err := es.client.IndexExists(name).Do(ctx)
	if err != nil {
		log.WithFields(log.Fields{"index": name}).WithError(err).Error("Failed to check for existing index")
		return
	}
	if exists {
		log.WithFields(log.Fields{"index": name}).Debug("Index already exists")
	} else {
		log.WithFields(log.Fields{"index": name}).Debug("Creating index")

		newIndex, err := es.client.CreateIndex(name).BodyString(testMapping).Do(ctx)
		if err != nil {
			log.WithFields(log.Fields{"index": name}).WithError(err).Error("Failed to create index")
			return
		}

		if !newIndex.Acknowledged {
			log.WithFields(log.Fields{"index": name}).Debug("Index not acknowledged")
			return
		}
	}
}
