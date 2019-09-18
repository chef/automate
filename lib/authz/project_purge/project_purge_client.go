package project_purge

import (
	"context"
)

const (
	maxNumberOfConsecutiveFails = 10
)

type PurgeClient interface {
	PurgeProject(context.Context, string) error
}
