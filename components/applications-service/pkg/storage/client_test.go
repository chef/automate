package storage

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/external/habitat"
)

// TestUpdateStrategiesInSyncBetweenAutomateAndHabitat verifies that the code
// in this package is up-to-date with the update strategies described by the
// habitat protobuf file.
func TestUpdateStrategiesInSyncBetweenAutomateAndHabitat(t *testing.T) {
	// At the time of writing, habitat defines:
	//
	//   UpdateStrategy_AtOnce UpdateStrategy = 0
	//   UpdateStrategy_Rolling UpdateStrategy = 1
	//
	// If a new one is added, then we need to update our corresponding UpdateStrategy constants

	// The highest integer-value update strategy that Automate code knows about
	// should exist in the map that the protobuf code generator made:
	maxHabUpdateStrategyInt := int32(1)
	_, isKnown := habitat.UpdateStrategy_name[maxHabUpdateStrategyInt]
	assert.True(t, isKnown)

	for i := int32(0); i <= maxHabUpdateStrategyInt; i++ {
		habStrategy := habitat.UpdateStrategy(i)
		storageFormattedStrategy := HabitatUpdateStrategyToStorageFormat(habStrategy)
		assert.NotEqual(t, updateStrategyUnrecognizedName, storageFormattedStrategy.String())
	}

	// Check the next 10 integer values, they should not be known to the hab
	// protobuf code. We check several in case some values got reserved.
	for i := int32(1); i <= 10; i++ {
		index := maxHabUpdateStrategyInt + i
		val, isKnown := habitat.UpdateStrategy_name[index]
		assert.False(t, isKnown, "expected to not find value %q for update strategy index %d", val, index)
	}
}

func TestUpdateStrategiesHaveAStringDefined(t *testing.T) {
	// ensure all update strategies have an entry in the case statement for the String() method
	for i := 0; i < int(UnrecognizedStrategy); i++ {
		str := UpdateStrategy(i).String()
		assert.NotEqual(t, updateStrategyUnrecognizedName, str)
	}
}

func TestUpdateStrategyFromTheFutureIsUNRECOGNIZED(t *testing.T) {
	nextHabUpdateStrategyInt := int32(2)
	nextHabUpdateStrategy := habitat.UpdateStrategy(nextHabUpdateStrategyInt)

	storageFormattedStrategy := HabitatUpdateStrategyToStorageFormat(nextHabUpdateStrategy)
	assert.Equal(t, updateStrategyUnrecognizedName, storageFormattedStrategy.String())
}
