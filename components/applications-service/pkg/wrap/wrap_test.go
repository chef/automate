package wrap

import (
	"fmt"
	"testing"

	"github.com/gogo/protobuf/proto"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/applications"
)

func TestWrap(t *testing.T) {
	ex1 := applications.ExampleOne{
		IntOne: 23,
		IntTwo: 42,
	}

	packedEx1, err := proto.Marshal(&ex1)
	require.NoError(t, err)

	wrap1 := applications.Wrapper{
		Type:    "ExampleOne",
		Message: packedEx1,
	}

	fmt.Printf("w: %+v\n", wrap1)

	data, err := proto.Marshal(&wrap1)

	require.NoError(t, err)

	fmt.Printf("data: %+v\n", data)

	var wrap1rt applications.Wrapper

	err = proto.Unmarshal(data, &wrap1rt)

	require.NoError(t, err)

	fmt.Printf("w (rt): %+v\n", wrap1rt)

	var extractedEx1 applications.ExampleOne

	err = proto.Unmarshal(wrap1rt.Message, &extractedEx1)

	require.NoError(t, err)

	fmt.Printf("ex1 (rt): %+v\n", extractedEx1)
}
