package toml

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/pkg/errors"

	toml "github.com/chef/toml"
)

// Unmarshal parses the TOML encoded data and stores the result in the
// value pointed to by v.
func Unmarshal(data []byte, v interface{}) error {
	return toml.Unmarshal(data, v)
}

// StrictUnmarshal parses the TOML encoded data and stores the result
// in the value pointed to by v. Unlike Unmarshal, any unknown keys in
// the data is an error.
func StrictUnmarshal(data []byte, v interface{}) error {
	md, err := toml.Decode(string(data), v)
	if err != nil {
		return err
	}

	undecoded := md.Undecoded()
	if len(undecoded) > 0 {
		msg := strings.Builder{}
		if len(undecoded) == 1 {
			msg.WriteString(fmt.Sprintf("unknown configuration key %q", undecoded[0]))
		} else {
			msg.WriteString("unknown configuration keys:")
			for _, v := range undecoded {
				msg.WriteString("\n    ")
				msg.WriteString(v.String())
			}
		}
		return errors.New(msg.String())
	}

	return nil
}

// Marshal returns the TOML encoding of v.
func Marshal(v interface{}) ([]byte, error) {
	buf := new(bytes.Buffer)
	enc := toml.NewEncoder(buf)
	err := enc.Encode(v)
	return buf.Bytes(), err
}

// Validate returns nil if data is valid TOML. Otherwise, an error
// describing the parse failure is returned.
func Validate(data []byte) error {
	var tmp interface{}
	err := toml.Unmarshal(data, &tmp)
	return err
}
