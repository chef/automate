package deployment

import (
	"fmt"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
)

// StatusTimeoutError occurs when
// not all services come up as expected
type StatusTimeoutError struct {
	Status  *api.ServiceStatus
	Timeout time.Duration
}

var statusTimeoutErrorMessage = `Chef Automate services failed to start in %d seconds.

Service Status:

%s
For more information check the system logs:

    journalctl -u chef-automate`

func (e *StatusTimeoutError) Error() string {
	return fmt.Sprintf(statusTimeoutErrorMessage, int(e.Timeout.Seconds()), e.Status.FormatStatus())
}
