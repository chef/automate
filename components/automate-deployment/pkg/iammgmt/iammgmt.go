package iammgmt

import (
	"context"
)

// IamMgmt is the interface that upgrades IAM to v2 on fresh installs
type IamMgmt interface {
	UpgradeToV2(ctx context.Context) error
}
