package v2

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestFindLongestEstimatedTimeEmpty(t *testing.T) {
	estimateFound := findLongestEstimate([]domainEstimate{})

	assert.True(t, estimateFound.time.IsZero())
}

func TestFindLongestEstimatedTimeEstimatesInFuture(t *testing.T) {
	estimates := make([]domainEstimate, 5)
	for i := 0; i < 5; i++ {
		estimates[i].time = time.Now().Add(time.Hour * time.Duration(i))
	}

	estimateFound := findLongestEstimate(estimates)

	assert.Equal(t, estimates[4], estimateFound)
}

func TestFindLongestEstimatedTimeEstimatesInPast(t *testing.T) {
	estimates := make([]domainEstimate, 5)
	for i := 0; i < 5; i++ {
		estimates[i].time = time.Now().Add(time.Hour * time.Duration(-1*i))
	}

	estimateFound := findLongestEstimate(estimates)

	assert.True(t, estimateFound.time.IsZero())
}

func TestFindLongestEstimatedTimeEstimatesInPastAndFuture(t *testing.T) {
	estimates := make([]domainEstimate, 10)
	for i := 0; i < 5; i++ {
		estimates[i].time = time.Now().Add(time.Hour * time.Duration(-1*i))
	}

	for i := 5; i < 10; i++ {
		estimates[i].time = time.Now().Add(time.Hour * time.Duration(1*i))
	}

	estimateFound := findLongestEstimate(estimates)

	assert.Equal(t, estimates[9], estimateFound)
}
