package chef

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
)

// JSONReader handles arbitrary types and synthesizes a streaming encoder for them.
func JSONReader(v interface{}) (r io.Reader, err error) {
	if debug_on() {
		jsonout, err := json.Marshal(v)
		fmt.Printf("\n\nJSON IN: %+v \n JSON ERR: %+v\n", string(jsonout), err)
	}
	buf := new(bytes.Buffer)
	err = json.NewEncoder(buf).Encode(v)
	r = bytes.NewReader(buf.Bytes())
	return
}
