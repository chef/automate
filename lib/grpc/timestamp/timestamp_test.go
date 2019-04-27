package timestamp

import (
	"testing"

	"github.com/stretchr/testify/assert"
	tspb "github.com/golang/protobuf/ptypes/timestamp"
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

