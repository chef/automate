package a1upgrade

import (
	"testing"

	"github.com/olivere/elastic"
	"github.com/stretchr/testify/assert"
)

func indicesSettingsForIndex(idx indexInfo) *elastic.IndicesGetSettingsResponse {
	s := make(map[string]interface{})
	v := make(map[string]interface{})
	c := make(map[string]interface{})

	c["created"] = idx.created
	v["version"] = c
	s["index"] = v

	return &elastic.IndicesGetSettingsResponse{Settings: s}
}

const oldCreatedNumber = "2040199"
const newCreatedNumber = "5040199"

func TestAddIndexToSortedIndices(t *testing.T) {
	t.Run("Adding an old insights-YYYY-MM-DD index adds it to the insights to-migrate list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: "insights-2018.04.17", created: oldCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.Contains(t, list.insightsIndicesToMigrate, idx)
		assert.NotContains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})
	t.Run("Adding a new insights-YYYY-MM-DD index doesn't add it to the insights to-migrate list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: "insights-2018.04.17", created: newCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.NotContains(t, list.insightsIndicesToMigrate, idx)
		assert.Contains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})
	t.Run("Adding an old compliance-YYYY-MM-DD index adds it to the compliance to-migrate list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: "compliance-2018.04.17", created: oldCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.Contains(t, list.complianceIndicesToMigrate, idx)
		assert.NotContains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})
	t.Run("Adding a new compliance-YYYY-MM-DD index doesn't add it to the compliance to-migrate list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: "compliance-2018.04.17", created: newCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.NotContains(t, list.complianceIndicesToMigrate, idx)
		assert.Contains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})
	t.Run("Adding an old .automate index adds it to the other A1 indices to-migrate list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: ".automate", created: oldCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.Contains(t, list.otherA1Indices, idx)
		assert.NotContains(t, list.complianceIndicesToMigrate, idx)
		assert.NotContains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})
	t.Run("Adding a new .automate index doesn't add it to the other a1 indices to-migrate list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: ".automate", created: newCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.NotContains(t, list.otherA1Indices, idx)
		assert.NotContains(t, list.complianceIndicesToMigrate, idx)
		assert.Contains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})
	t.Run("Adding an old saved-searches index adds it to the other A1 indices to-migrate list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: "saved-searches", created: oldCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.Contains(t, list.otherA1Indices, idx)
		assert.NotContains(t, list.complianceIndicesToMigrate, idx)
		assert.NotContains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})
	t.Run("Adding a new saved-searches index doesn't add it to the other a1 indices to-migrate list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: "saved-searches", created: newCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.NotContains(t, list.otherA1Indices, idx)
		assert.NotContains(t, list.complianceIndicesToMigrate, idx)
		assert.Contains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})

	t.Run("Adding an old index that doesn't match a known name adds it to the unknown list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: "whatsis", created: oldCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.NotContains(t, list.otherA1Indices, idx)
		assert.NotContains(t, list.complianceIndicesToMigrate, idx)
		assert.NotContains(t, list.compatibleIndices, idx)
		assert.Contains(t, list.unknownIndices, idx)
	})

	t.Run("Adding a new index that doesn't match a known name doesn't add it to the unknown list", func(t *testing.T) {
		list := newSortedIndices()

		idx := indexInfo{name: "whatsis", created: newCreatedNumber}

		list.addIndex(idx.name, indicesSettingsForIndex(idx))

		assert.NotContains(t, list.otherA1Indices, idx)
		assert.NotContains(t, list.complianceIndicesToMigrate, idx)
		assert.Contains(t, list.compatibleIndices, idx)
		assert.NotContains(t, list.unknownIndices, idx)
	})
}
