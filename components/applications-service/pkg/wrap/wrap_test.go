package wrap

import (
	"fmt"
	"testing"

	"github.com/golang/protobuf/proto"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/external/applications"
)

// Shows at the most basic level what round-tripping a wrapped message looks
// like.
func TestBasicWrap(t *testing.T) {
	ex1 := applications.ExampleOne{
		IntOne: 23,
		IntTwo: 42,
	}

	packedEx1, err := proto.Marshal(&ex1)
	require.NoError(t, err)

	fmt.Printf("inner msg: %+v\n", packedEx1)
	fmt.Printf("inner msg len: %d\n", len(packedEx1))

	wrap1 := applications.Wrapper{
		Type:    applications.Wrapper_EXAMPLE_ONE,
		Message: packedEx1,
	}

	fmt.Printf("w: %+v\n", wrap1)

	data, err := proto.Marshal(&wrap1)

	require.NoError(t, err)

	fmt.Printf("data: %+v\n", data)
	fmt.Printf("data len: %d\n", len(data))

	var wrap1rt applications.Wrapper

	err = proto.Unmarshal(data, &wrap1rt)

	require.NoError(t, err)

	fmt.Printf("w (rt): %+v\n", wrap1rt)

	var extractedEx1 applications.ExampleOne

	err = proto.Unmarshal(wrap1rt.Message, &extractedEx1)

	require.NoError(t, err)

	fmt.Printf("ex1 (rt): %+v\n", extractedEx1)
}

// A bit more complete example that shows a basic shape of code we could have
// if we use the wrapper message approach
func TestTypeBasedDispatch(t *testing.T) {
	ex1 := applications.ExampleOne{
		IntOne: 23,
		IntTwo: 42,
	}

	packedEx1, err := proto.Marshal(&ex1)
	require.NoError(t, err)

	wrap1 := applications.Wrapper{
		Type:    applications.Wrapper_EXAMPLE_ONE,
		Message: packedEx1,
	}

	data1, err := proto.Marshal(&wrap1)
	require.NoError(t, err)

	ex2 := applications.ExampleTwo{
		StringOne: "hello",
		StringTwo: "world",
	}

	packedEx2, err := proto.Marshal(&ex2)
	require.NoError(t, err)

	wrap2 := applications.Wrapper{
		Type:    applications.Wrapper_EXAMPLE_TWO,
		Message: packedEx2,
	}

	data2, err := proto.Marshal(&wrap2)
	require.NoError(t, err)

	dispatchWrappedMessage(data1, t)
	dispatchWrappedMessage(data2, t)
}

func dispatchWrappedMessage(m []byte, t *testing.T) {
	var wrapper applications.Wrapper
	err := proto.Unmarshal(m, &wrapper)
	require.NoError(t, err)

	switch wrapper.Type {
	case applications.Wrapper_EXAMPLE_ONE:
		var innerMsg applications.ExampleOne
		err = proto.Unmarshal(wrapper.Message, &innerMsg)
		require.NoError(t, err)
		handleExOneMessage(innerMsg)
	case applications.Wrapper_EXAMPLE_TWO:
		var innerMsg applications.ExampleTwo
		err = proto.Unmarshal(wrapper.Message, &innerMsg)
		require.NoError(t, err)
		handleExTwoMessage(innerMsg)
	default:
		fmt.Printf("don't know what to do with message %+v\n", wrapper)
	}
}

func handleExOneMessage(m applications.ExampleOne) {
	fmt.Printf("got an ExampleOne message %+v\n", m)
}

func handleExTwoMessage(m applications.ExampleTwo) {
	fmt.Printf("got an ExampleTwo message %+v\n", m)
}
