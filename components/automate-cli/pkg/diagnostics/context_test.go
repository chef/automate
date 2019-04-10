package diagnostics_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
)

type Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

const jsonData = `
{
	"components": {
		"componentA": {
			"name": "bob",
			"age": 99
		}
	}
}
`

var structValue = Person{Name: "bob", Age: 99}

func TestReadTestContext(t *testing.T) {
	tstCtx, err := diagnostics.LoadTestContext(nil, strings.NewReader(jsonData))
	require.NoError(t, err)
	person := Person{}
	err = tstCtx.GetValue("componentA", &person)

	require.NoError(t, err)
	assert.Equal(t, structValue, person)
}

func TestReadTestContextNoKey(t *testing.T) {
	tstCtx := diagnostics.NewTestContext(nil)
	person := Person{}
	err := tstCtx.GetValue("foo", &person)
	assert.Equal(t, diagnostics.ErrKeyNotFound, err)
}

func TestRoundTripTestContext(t *testing.T) {
	tstCtx := diagnostics.NewTestContext(nil)
	tstCtx.SetValue("componentB", structValue)
	buf := bytes.NewBufferString("")
	err := tstCtx.WriteJSON(buf)
	require.NoError(t, err)

	tstCtx, err = diagnostics.LoadTestContext(nil, buf)
	require.NoError(t, err)
	person := Person{}
	err = tstCtx.GetValue("componentB", &person)

	require.NoError(t, err)
	assert.Equal(t, structValue, person)
}
