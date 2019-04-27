package timestamp

import (
	"testing"

	"github.com/stretchr/testify/assert"
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

func TestTimestampString(t *testing.T) {
	for _, test := range []struct {
		ts       *tspb.Timestamp
		expected string
	}{
		// very basic test
		{&tspb.Timestamp{Seconds: 0, Nanos: 0}, "1970-01-01T00:00:00Z"},
		// make sure we dont add nanos to the output
		{&tspb.Timestamp{Seconds: 0, Nanos: 9}, "1970-01-01T00:00:00Z"},
		// test nil
		{nil, ""},
		// test invalid (negative time, extra time)
		{&tspb.Timestamp{Seconds: minValidSeconds - 1, Nanos: 0}, ""},
		{&tspb.Timestamp{Seconds: maxValidSeconds + 1, Nanos: 0}, ""},
	} {
		assert.Equal(t, test.expected, TimestampString(test.ts))
	}
}

