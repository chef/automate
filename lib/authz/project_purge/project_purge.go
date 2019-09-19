package project_purge

import (
	"context"
	"math"
	"time"
)

const (
	maxNumberOfConsecutiveFails = 10
)

type PurgeClient interface {
	PurgeProject(context.Context, string) error
}

func ExponentialNextCheck(numberOfFailures int) time.Time {
	// use exponential backoff per retry of numberOfFailure^2 up until a max.
	nextDuration := (time.Second * time.Duration(math.Pow(float64(numberOfFailures), 2.0)))
	if nextDuration > maxBackoffDuration {
		nextDuration = maxBackoffDuration
	}
	return time.Now().Add(nextDuration)
}
