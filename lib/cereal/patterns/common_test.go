package patterns

import (
	"encoding/json"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type A struct {
	AField string
}
type B struct {
	BField string
}

func jsonRoundTrip(t *testing.T, in interface{}, out interface{}) {
	t.Helper()

	d, err := json.Marshal(in)
	require.NoError(t, err)

	err = json.Unmarshal(d, &out)
	require.NoError(t, err)
}

func TestAddMetadataToStruct(t *testing.T) {
	t.Run("happy path structs", func(t *testing.T) {
		a := A{
			AField: "afield",
		}
		b := B{
			BField: "bfield",
		}

		mergedObj, err := merge(a, b)
		require.NoError(t, err)

		newA := A{}
		newB := B{}

		jsonRoundTrip(t, mergedObj, &newA)
		jsonRoundTrip(t, mergedObj, &newB)
		assert.Equal(t, a, newA)
		assert.Equal(t, b, newB)
	})

	t.Run("happy path pointers", func(t *testing.T) {
		a := &A{
			AField: "afield",
		}
		b := &B{
			BField: "bfield",
		}

		mergedObj, err := merge(a, b)
		require.NoError(t, err)

		newA := &A{}
		newB := &B{}

		jsonRoundTrip(t, mergedObj, &newA)
		jsonRoundTrip(t, mergedObj, &newB)
		assert.Equal(t, a, newA)
		assert.Equal(t, b, newB)
	})

	t.Run("field 1 nil interface", func(t *testing.T) {
		var a *A

		b := &B{
			BField: "bfield",
		}

		mergedObj, err := merge(a, b)
		require.NoError(t, err)

		newA := A{}
		newB := &B{}

		jsonRoundTrip(t, mergedObj, &newA)
		jsonRoundTrip(t, mergedObj, &newB)
		assert.Equal(t, "", newA.AField)
		assert.Equal(t, b, newB)
	})

	t.Run("field 1 nil", func(t *testing.T) {
		b := &B{
			BField: "bfield",
		}

		mergedObj, err := merge(nil, b)
		require.NoError(t, err)

		newA := A{}
		newB := &B{}

		jsonRoundTrip(t, mergedObj, &newA)
		jsonRoundTrip(t, mergedObj, &newB)
		assert.Equal(t, "", newA.AField)
		assert.Equal(t, b, newB)
	})

	t.Run("field 2 nil interface", func(t *testing.T) {
		var a *A

		b := &B{
			BField: "bfield",
		}

		mergedObj, err := merge(b, a)
		require.NoError(t, err)

		newA := A{}
		newB := &B{}

		jsonRoundTrip(t, mergedObj, &newA)
		jsonRoundTrip(t, mergedObj, &newB)
		assert.Equal(t, "", newA.AField)
		assert.Equal(t, b, newB)
	})

	t.Run("field 2 nil", func(t *testing.T) {
		b := &B{
			BField: "bfield",
		}

		mergedObj, err := merge(b, nil)
		require.NoError(t, err)

		newA := A{}
		newB := &B{}

		jsonRoundTrip(t, mergedObj, &newA)
		jsonRoundTrip(t, mergedObj, &newB)
		assert.Equal(t, "", newA.AField)
		assert.Equal(t, b, newB)
	})

	t.Run("invalid", func(t *testing.T) {
		a := &A{}
		invalid := []interface{}{
			[]string{"foo"},
			&[]string{"foo"},
			int(1),
			int16(1),
			int32(1),
			int64(1),
			uint(1),
			int16(1),
			uint32(1),
			uint64(1),
			float32(1),
			float64(1),
			&a,
		}

		for _, i := range invalid {
			_, err := merge(a, i)
			require.Equal(t, ErrCannotMergeTypes, err)
			_, err = merge(i, a)
			require.Equal(t, ErrCannotMergeTypes, err)
		}

	})
}
