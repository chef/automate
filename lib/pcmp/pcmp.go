package pcmp

import (
	"bufio"
	"fmt"
	"reflect"
	"time"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"google.golang.org/protobuf/runtime/protoimpl"
)

// compareOptsForProtos is a slice of options for the
// https://github.com/google/go-cmp/cmp object comparing package. As of the
// latest implementation of the protocompiler for go, the messages compile to something like:
//  type YourMessage struct {
//  	state         protoimpl.MessageState
//  	sizeCache     protoimpl.SizeCache
//  	unknownFields protoimpl.UnknownFields
//
//  // Your fields here
//  }
// Comparisons on the private fields often/always fail (this seems intentional
// on the part of the protobuf devs). Excluding these types from comparison
// allows us to do deep equality tests on just the public fields we defined and
// skip internal proto stuff.
//
// We also use cmpopts.EquateEmpty, which makes a nil map and an empty one
// equal, because the other packages we have been using for deep equality
// comparisons behave this way.
var CompareOptsForProtos = []cmp.Option{cmpopts.IgnoreTypes(
	protoimpl.MessageState{},
	protoimpl.SizeCache(0),
	protoimpl.UnknownFields{},
),
	cmpopts.EquateEmpty(),
}

func DeepEqual(x, y interface{}) bool {
	return cmp.Equal(x, y, CompareOptsForProtos...)
}

func FormatUnequalValues(expected, actual interface{}) (e string, a string) {
	if reflect.TypeOf(expected) != reflect.TypeOf(actual) {
		return fmt.Sprintf("%T(%s)", expected, truncatingFormat(expected)),
			fmt.Sprintf("%T(%s)", actual, truncatingFormat(actual))
	}
	switch expected.(type) {
	case time.Duration:
		return fmt.Sprintf("%v", expected), fmt.Sprintf("%v", actual)
	}
	return truncatingFormat(expected), truncatingFormat(actual)
}

func truncatingFormat(data interface{}) string {
	value := fmt.Sprintf("%#v", data)
	max := bufio.MaxScanTokenSize - 100 // Give us some space the type info too if needed.
	if len(value) > max {
		value = value[0:max] + "<... truncated>"
	}
	return value
}
