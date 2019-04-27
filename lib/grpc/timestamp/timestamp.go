package timestamp

import (
	"time"

	"github.com/golang/protobuf/ptypes"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
)

const (
	// Seconds field of the earliest valid Timestamp.
	// This is time.Date(1, 1, 1, 0, 0, 0, 0, time.UTC).Unix().
	minValidSeconds = -62135596800

	// Seconds field just after the latest valid Timestamp.
	// This is time.Date(10000, 1, 1, 0, 0, 0, 0, time.UTC).Unix().
	maxValidSeconds = 253402300800
)

// TimestampString: return a string representation of the ptype timestamp

// this is inspired by the ptypes.TimestampString() function that
// encodes errors into the string when receiving a nil or mis-typed
// timestamp.  it also returns the string in RFC3339 format instead of
// RFC3339nano format in order to keep compatiblity w/ the current
// behavior
func TimestampString(ts *tspb.Timestamp) string {
	t, err := ptypes.Timestamp(ts)
	if err != nil {
		return ""
	}
	return t.Format(time.RFC3339)
}
