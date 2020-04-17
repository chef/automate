package a1upgrade

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"time"

	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"gopkg.in/cheggaaa/pb.v1"
	elastic "gopkg.in/olivere/elastic.v6"

	"github.com/chef/automate/components/automate-deployment/pkg/assets"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/stringutils"
)

var knownA1IndexNames = []string{"saved-searches", ".automate"}

type Reindexer struct {
	es            *elastic.Client
	w             cli.FormatWriter
	sortedIndices *sortedIndices
}

type sortedIndices struct {
	allIndices                 map[string]bool
	complianceIndicesToMigrate []indexInfo
	insightsIndicesToMigrate   []indexInfo
	otherA1Indices             []indexInfo
	compatibleIndices          []indexInfo
	unknownIndices             []indexInfo
}

func NewReindexer(w cli.FormatWriter, elasticsearchURL string) (*Reindexer, error) {
	e, err := elastic.NewSimpleClient(elastic.SetURL(elasticsearchURL))
	if err != nil {
		return nil, err
	}

	return &Reindexer{w: w, es: e}, nil
}

func (r *Reindexer) DiskRequirementsInBytes() (largestIndexBytes int64, err error) {
	log.Debug("analyzing Es indices to determine necessary disk space required to reindex es2 indices")
	var largestIndexInBytes int64

	sortedIndices, err := r.listIndices()
	if err != nil {
		return largestIndexInBytes, err
	}
	sortedIndices.logReport()

	allIndexNames := []string{}

	// compliance and insights get reindexed, which will temporarily duplicate them;
	// other "known" a1 indices are deleted so we don't care about their size
	for _, indexInfo := range sortedIndices.insightsIndicesToMigrate {
		allIndexNames = append(allIndexNames, indexInfo.name)
	}
	for _, indexInfo := range sortedIndices.complianceIndicesToMigrate {
		allIndexNames = append(allIndexNames, indexInfo.name)
	}

	// the real Es doesn't need these to be sorted, but it makes it easier to
	// write the stub Es API in the a1stub package
	sort.Strings(allIndexNames)

	// If we pass an empty list of indices to IndexStats, we get stats for
	// everything, and then we find the largest index regardless of es6 compat,
	// so we need to return early if no indices need reindexing
	if len(allIndexNames) == 0 {
		log.Debug("Found no es6 incompatible indices, don't need extra disk space to support reindexing")
		return largestIndexInBytes, nil
	}

	log.WithFields(log.Fields{"index-count": len(allIndexNames), "index-names": allIndexNames}).Debug("querying es for on-disk index sizes")

	partitionedNames := partitionIndexNames(allIndexNames)
	responses := []*elastic.IndicesStatsResponse{}

	for _, idxSubset := range partitionedNames {
		statsFetcher := r.es.IndexStats(idxSubset...).Metric("store")
		ctx := context.Background()
		res, err := statsFetcher.Do(ctx)

		if err != nil {
			return largestIndexInBytes, errors.Wrapf(err, "failed to collect index size stats for es2 format indices (%s)", strings.Join(idxSubset, ","))
		}

		responses = append(responses, res)
	}

	var largestIndexName string

	for _, res := range responses {
		for indexName, stats := range res.Indices {
			s := stats.Primaries.Store.SizeInBytes
			log.WithFields(log.Fields{"index-name": indexName, "size-in-bytes": s}).Debug("found es2 formatted index")
			if s > largestIndexInBytes {
				largestIndexInBytes = s
				largestIndexName = indexName
			}
		}
	}

	log.WithFields(log.Fields{"index-name": largestIndexName, "size-in-bytes": largestIndexInBytes}).Info("found largest es6 incompatible index")

	return largestIndexInBytes, nil
}

func (r *Reindexer) UnknownIndicesError() error {
	sortedIndices, err := r.listIndices()
	if err != nil {
		return err
	}
	sortedIndices.logReport()
	return sortedIndices.unknownIndicesError()
}

func (r *Reindexer) RunReindex() error {
	sortedIndices, err := r.listIndices()
	if err != nil {
		return err
	}
	sortedIndices.logReport()
	err = sortedIndices.unknownIndicesError()
	if err != nil {
		return err
	}

	insightsIndicesCount := len(sortedIndices.insightsIndicesToMigrate)
	if insightsIndicesCount > 0 {
		r.w.Bodyf("Reindexing %d insights indices for Elasticsearch 6 compatibility", insightsIndicesCount)
		bar := pb.StartNew(insightsIndicesCount)
		for _, indexInfo := range sortedIndices.insightsIndicesToMigrate {
			if err := r.insightsMigration(indexInfo.name); err != nil {
				bar.Finish()
				return err
			}
			bar.Increment()
		}
		bar.Finish()
	}

	complianceIndicesCount := len(sortedIndices.complianceIndicesToMigrate)
	if complianceIndicesCount > 0 {
		r.w.Bodyf("Reindexing %d compliance indices for Elasticsearch 6 compatibility", complianceIndicesCount)
		bar := pb.StartNew(complianceIndicesCount)
		for _, indexInfo := range sortedIndices.complianceIndicesToMigrate {
			if err := r.complianceMigration(indexInfo.name); err != nil {
				bar.Finish()
				return err
			}
			bar.Increment()
		}
		bar.Finish()
	}
	otherIndicesCount := len(sortedIndices.otherA1Indices)
	if otherIndicesCount > 0 {
		r.w.Bodyf("Removing %d obsoleted indices", otherIndicesCount)
		bar := pb.StartNew(otherIndicesCount)
		for _, indexInfo := range sortedIndices.otherA1Indices {
			if err := r.removeObsoleteIndex(indexInfo.name); err != nil {
				bar.Finish()
				return err
			}
			bar.Increment()
		}
		bar.Finish()
	}
	return nil
}

func (r *Reindexer) listIndices() (*sortedIndices, error) {
	if r.sortedIndices != nil {
		return r.sortedIndices, nil
	}

	names, err := r.es.IndexNames()
	if err != nil {
		return nil, errors.Wrap(err, "failed to list es indexes while looking for es2 indices")
	}
	log.WithFields(log.Fields{"total-indices": len(names)}).Info("analyzing indices for Elasticsearch 6 compatibility")

	// the real Es doesn't need these to be sorted, but it makes it easier to
	// write the stub Es API in the a1stub package
	sort.Strings(names)

	partitionedNames := partitionIndexNames(names)

	indices := newSortedIndices()

	for _, namesToCheck := range partitionedNames {

		err := r.listIndicesByName(indices, namesToCheck...)
		if err != nil {
			return nil, err
		}
	}

	r.sortedIndices = indices
	return indices, nil
}

func (r *Reindexer) listIndicesByName(indices *sortedIndices, list ...string) error {
	b := context.Background()
	settingsReq := r.es.IndexGetSettings(list...)
	settingsResponse, err := settingsReq.Do(b)
	if err != nil {
		return errors.Wrap(err, "failed to read es indices settings while looking for es2 indices")
	}

	for index, indexSettingsResponse := range settingsResponse {
		if err := indices.addIndex(index, indexSettingsResponse); err != nil {
			return err
		}
	}
	return nil
}

func (r *Reindexer) insightsMigration(srcIndex string) error {
	log.WithFields(log.Fields{"index-name": srcIndex}).Info("migrating insights index")
	return r.reindexWithNewMapping(srcIndex, assets.A1InsightsIndexMapping)
}

func (r *Reindexer) complianceMigration(srcIndex string) error {
	log.WithFields(log.Fields{"index-name": srcIndex}).Info("migrating compliance index")
	err := r.reindexWithNewMapping(srcIndex, assets.A1ComplianceIndexMapping)
	if err != nil {
		return err
	}
	return r.aliasOldIndex(srcIndex)
}

func replacementIndexName(oldIndexName string) (newIndexName string) {
	return fmt.Sprintf("%s-1", oldIndexName)
}

func (r *Reindexer) reindexWithNewMapping(srcIndex, mapping string) error {
	ctx := context.Background()

	destIndex := replacementIndexName(srcIndex)
	_, err := r.es.CreateIndex(destIndex).BodyString(mapping).Do(ctx)
	if err != nil {
		return errors.Wrapf(err, "failed to create destination index '%s' for reindexing '%s'", destIndex, srcIndex)
	}
	// https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-reindex.html#docs-reindex-automatic-slice
	// https://godoc.org/github.com/olivere/elastic#ReindexService
	// Es provides a slices setting which theoretically should speed up the
	// reindex process by parallelizing it, but local testing did not show any
	// improvement. It may be useful on more production-like systems, depending
	// on the configuration. It can be enabled by adding `.Slices(n=int)` to the
	// method chain here:
	startTaskResult, err := r.es.Reindex().SourceIndex(srcIndex).DestinationIndex(destIndex).Refresh("true").DoAsync(ctx)
	if err != nil {
		return errors.Wrapf(err, "failed to reindex source index '%s' to destination '%s'", srcIndex, destIndex)
	}

	// Wait for Reindex task to complete
	for {
		time.Sleep(time.Millisecond * 500)

		tasksGetTaskResponse, err := elastic.NewTasksGetTaskService(r.es).
			TaskId(startTaskResult.TaskId).
			WaitForCompletion(false).
			Do(ctx)
		if err != nil {
			return err
		}

		if tasksGetTaskResponse.Completed {
			break
		}
	}

	deleteResponse, err := r.es.DeleteIndex(srcIndex).Do(ctx)
	if err != nil {
		return errors.Wrapf(err, "failed to remove index '%s' after reindexing to '%s'", srcIndex, destIndex)
	}
	if !deleteResponse.Acknowledged {
		return errors.Errorf("Es didn't ack request to remove index '%s' after reindexing to '%s'", srcIndex, destIndex)
	}
	return nil
}

func (r *Reindexer) aliasOldIndex(srcIndex string) error {
	newIndexName := replacementIndexName(srcIndex)
	ctx := context.Background()
	_, err := r.es.Alias().Add(newIndexName, srcIndex).Do(ctx)
	if err != nil {
		return errors.Wrapf(err, "failed to add alias to original index name '%s' after reindexing to '%s'", srcIndex, newIndexName)
	}
	return nil
}

func (r *Reindexer) removeObsoleteIndex(srcIndex string) error {
	log.WithFields(log.Fields{"index-name": srcIndex}).Info("removing obsolete a1 index")
	ctx := context.Background()
	deleteResponse, err := r.es.DeleteIndex(srcIndex).Do(ctx)
	if err != nil {
		return errors.Wrapf(err, "failed to remove obsolete elasticsearch index '%s'", srcIndex)
	}
	if !deleteResponse.Acknowledged {
		return errors.Errorf("Es didn't ack request to remove obsolete index '%s'", srcIndex)
	}
	return nil
}

type indexInfo struct {
	name    string
	created string
}

func newSortedIndices() *sortedIndices {
	s := sortedIndices{allIndices: make(map[string]bool)}
	return &s
}

func (l *sortedIndices) knownIndexName(indexName string) bool {
	return stringutils.SliceContains(knownA1IndexNames, indexName)
}

func (l *sortedIndices) addIndex(indexName string, settings *elastic.IndicesGetSettingsResponse) error {
	// uniq indices as they are added. I added this because the `--self-test`
	// mode returns static data that can cause duplicates, but other code in here
	// will fail if we have duplicates, so we might as well enforce it.
	if l.allIndices[indexName] {
		return nil
	}

	l.allIndices[indexName] = true

	created, err := l.extractIndexCreatedVersionFrom(settings)
	if err != nil {
		return errors.Wrapf(err, "unable to determine Elasticsearch version compatibility for index %s", indexName)
	}
	idx := indexInfo{name: indexName, created: created}

	if !strings.HasPrefix(created, "2") {
		l.compatibleIndices = append(l.compatibleIndices, idx)
		return nil
	}

	if strings.HasPrefix(indexName, "insights") {
		l.insightsIndicesToMigrate = append(l.insightsIndicesToMigrate, idx)
		return nil
	}
	if strings.HasPrefix(indexName, "compliance") {
		l.complianceIndicesToMigrate = append(l.complianceIndicesToMigrate, idx)
		return nil
	}
	if l.knownIndexName(indexName) {
		l.otherA1Indices = append(l.otherA1Indices, idx)
		return nil
	}

	l.unknownIndices = append(l.unknownIndices, idx)
	return nil
}

func (l *sortedIndices) extractIndexCreatedVersionFrom(res *elastic.IndicesGetSettingsResponse) (version string, err error) {
	settings := res.Settings
	maybeIndexSettings := settings["index"]
	indexSettings, err := extractMap(maybeIndexSettings)
	if err != nil {
		return "", errors.Wrapf(err, "Es returned incomplete settings data, cannot determine index created version (response data: %v)", settings)
	}
	versionSettings, err := extractMap(indexSettings["version"])
	if err != nil {
		return "", errors.Wrapf(err, "es returned incomplete settings data, cannot determine index created version (response data: %v)", settings)
	}

	created, err := extractStr(versionSettings["created"])
	if err != nil {
		return "", errors.Wrapf(err, "es returned settings data with unexpected format, cannot determine index created version (response data: %v)", settings)
	}
	return created, nil

}

func (l *sortedIndices) logReport() {
	if (len(l.insightsIndicesToMigrate) == 0) && (len(l.complianceIndicesToMigrate) == 0) {
		log.Info("All indices are Elasticsearch 6 compatible")
		return
	}
	log.WithFields(log.Fields{"indices-count": len(l.compatibleIndices)}).Debug("Found Elasticsearch 6 compatible indices")
	for _, idx := range l.compatibleIndices {
		log.WithFields(log.Fields{"index-name": idx.name, "es-created-version": idx.created}).Debug("es6 compatible index")
	}

	log.WithFields(log.Fields{"indices-count": len(l.insightsIndicesToMigrate)}).Info("Found Elasticsearch 6 incompatible insights indices")
	for _, idx := range l.insightsIndicesToMigrate {
		log.WithFields(log.Fields{"index-name": idx.name, "es-created-version": idx.created}).Debug("es6 incompatible insights index")
	}
	log.WithFields(log.Fields{"indices-count": len(l.complianceIndicesToMigrate)}).Info("Found Elasticsearch 6 incompatible compliance indices")
	for _, idx := range l.complianceIndicesToMigrate {
		log.WithFields(log.Fields{"index-name": idx.name, "es-created-version": idx.created}).Debug("es6 incompatible compliance index")
	}
	log.WithFields(log.Fields{"indices-count": len(l.otherA1Indices)}).Info("Found Elasticsearch 6 incompatible other a1 indices")
	for _, idx := range l.otherA1Indices {
		log.WithFields(log.Fields{"index-name": idx.name, "es-created-version": idx.created}).Debug("es6 incompatible other a1 index")
	}
}

func (l *sortedIndices) unknownIndicesError() error {
	if len(l.unknownIndices) != 0 {
		names := []string{}
		for _, indexInfo := range l.unknownIndices {
			names = append(names, indexInfo.name)
		}
		list := strings.Join(names, "', '")
		return errors.Errorf("found unknown indices that are not Elasticsearch 6 compatible: '%s' - remove or migrate these manually", list)
	}
	return nil
}

func extractMap(maybeMap interface{}) (map[string]interface{}, error) {
	if s, ok := maybeMap.(map[string]interface{}); ok {
		return s, nil
	} else {
		return nil, fmt.Errorf("expected %v (%T) to be a map", maybeMap, maybeMap)
	}
}

func extractStr(maybeString interface{}) (string, error) {
	if s, ok := maybeString.(string); ok {
		return s, nil
	} else {
		return "", fmt.Errorf("expected %v (%T) to be a string", maybeString, maybeString)
	}
}

func partitionIndexNames(names []string) [][]string {
	partitionedNames := [][]string{}
	chunkSize := 100
	chunkCount := len(names) / chunkSize

	for i := 0; i <= chunkCount; i++ {

		lowerBound := i * chunkSize
		upperBound := (i + 1) * chunkSize
		if upperBound > len(names) {
			upperBound = len(names)
		}
		if lowerBound == upperBound {
			continue
		}

		partitionedNames = append(partitionedNames, names[lowerBound:upperBound])
	}

	return partitionedNames
}
