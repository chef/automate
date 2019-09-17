package v2

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestFindLongestEstimatedTimeEmpty(t *testing.T) {
	estimateFound := findLongestEstimatedTime([]time.Time{})

	assert.True(t, estimateFound.IsZero())
}

func TestFindLongestEstimatedTimeEstimatesInFuture(t *testing.T) {
	estimates := make([]time.Time, 5)
	for i := 0; i < 5; i++ {
		estimates[i] = time.Now().Add(time.Hour * time.Duration(i))
	}

	estimateFound := findLongestEstimatedTime(estimates)

	assert.Equal(t, estimates[4], estimateFound)
}

func TestFindLongestEstimatedTimeEstimatesInPast(t *testing.T) {
	estimates := make([]time.Time, 5)
	for i := 0; i < 5; i++ {
		estimates[i] = time.Now().Add(time.Hour * time.Duration(-1*i))
	}

	estimateFound := findLongestEstimatedTime(estimates)

	assert.True(t, estimateFound.IsZero())
}

func TestFindLongestEstimatedTimeEstimatesInPastAndFuture(t *testing.T) {
	estimates := make([]time.Time, 10)
	for i := 0; i < 5; i++ {
		estimates[i] = time.Now().Add(time.Hour * time.Duration(-1*i))
	}

	for i := 5; i < 10; i++ {
		estimates[i] = time.Now().Add(time.Hour * time.Duration(1*i))
	}

	estimateFound := findLongestEstimatedTime(estimates)

	assert.Equal(t, estimates[9], estimateFound)
}
