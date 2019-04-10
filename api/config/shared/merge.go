package shared

import (
	"encoding/json"

	"github.com/golang/protobuf/jsonpb"
	proto "github.com/golang/protobuf/proto"
	"github.com/peterbourgon/mergemap"
)

// Merge merges `in` onto `base` and puts the result in `merged`. Note that
// `merged` should be pointer to a zero value for the struct type.
func Merge(base proto.Message, in proto.Message, merged proto.Message) error {
	baseMap, err := toMap(base)
	if err != nil {
		return err
	}
	inMap, err := toMap(in)
	if err != nil {
		return err
	}
	mergedMap := mergemap.Merge(baseMap, inMap)
	return fromMap(mergedMap, merged)
}

// toMap converts a struct to a map. The implementation does a
// roundtrip via json Marshal/Unmarshal.
func toMap(pb proto.Message) (map[string]interface{}, error) {
	// We could do this more directly with more effort, but the
	// inefficiency of the serialization roundtrip shouldn't be a
	// problem for our expected load. Preferring the JSON code as
	// protobuf generates good json struct tags and seems
	// reasonable to expect std lib code to be more reliable.
	marshaler := jsonpb.Marshaler{}
	raw, err := marshaler.MarshalToString(pb)
	if err != nil {
		return nil, err
	}
	m := make(map[string]interface{})
	err = json.Unmarshal([]byte(raw), &m)
	if err != nil {
		return nil, err
	}
	return m, nil
}

// fromMap converts a map to a struct.
func fromMap(m map[string]interface{}, pb proto.Message) error {
	raw, err := json.Marshal(m)
	if err != nil {
		return err
	}

	return jsonpb.UnmarshalString(string(raw), pb)
}
