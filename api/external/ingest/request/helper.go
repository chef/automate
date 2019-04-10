package request

import (
	"github.com/golang/protobuf/jsonpb"
)

// ToJSONString converts the Run message into a JSON String
// TODO @afiune Benchmark it!
func (run *Run) ToJSONString() (string, error) {
	return (&jsonpb.Marshaler{OrigName: true}).MarshalToString(run)
}

// ToJSONBytes converts the Run message into a JSON Bytes
func (run *Run) ToJSONBytes() ([]byte, error) {
	// This new field called 'content' is being used to send the entire raw JSON
	// message in bytes, this field is heavily used by the gateway for the DataCollector
	// Func Handler that will send the Run message to the (receiver) ingest-service
	// that will manually unmarshal the message from this field if it is provided.
	// The main purpose of this field it to improve the performance of ingestion when
	// the requests comes in REST/HTTP format.
	runJSONBytes := run.GetContent()
	if runJSONBytes == nil {
		runJSONString, err := run.ToJSONString()
		if err != nil {
			return []byte{}, err
		}
		runJSONBytes = []byte(runJSONString)
	}
	return runJSONBytes, nil
}
