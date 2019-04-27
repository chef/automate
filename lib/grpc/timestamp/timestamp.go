package timestamp

import (
	"time"

	"github.com/golang/protobuf/ptypes"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
)

// TimestampString: return a string representation of the ptype timestamp
// this is inspired by the ptypes.TimestampString() function that
// encodes errors into the string when receiving a nil or mis-typed
// timestamp.  it also returns the string in RFC3339 format instead of
// RFC3339nano format in order to keep compatibility w/ the current
// behavior
func TimestampString(ts *tspb.Timestamp) string {
	t, err := ptypes.Timestamp(ts)
	if err != nil {
		return ""
	}
	return t.Format(time.RFC3339)
}
