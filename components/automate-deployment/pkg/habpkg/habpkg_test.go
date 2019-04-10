package habpkg

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/stretchr/testify/assert"
)

func TestFromString(t *testing.T) {
	tests := []struct {
		in     string
		expRes HabPkg
		expErr error
	}{
		{
			"acme/pianos",
			HabPkg{"acme", "pianos", "", ""},
			nil,
		},
		{
			"acme/pianos/1.0",
			HabPkg{"acme", "pianos", "1.0", ""},
			nil,
		},
		{
			"acme/pianos/1.0/20180207070000",
			HabPkg{"acme", "pianos", "1.0", "20180207070000"},
			nil,
		},
		{
			"nosep",
			HabPkg{},
			errors.New("invalid package identifier 'nosep'"),
		},
		{
			"there/are/too/many/seps",
			HabPkg{},
			errors.New("invalid package identifier 'there/are/too/many/seps'"),
		},
	}

	for _, tt := range tests {
		sp, err := FromString(tt.in)
		assert.Equal(t, tt.expRes, sp)
		if tt.expErr != nil {
			assert.EqualError(t, err, tt.expErr.Error(), tt.expErr.Error())
		}
	}
}

func TestFromStrings(t *testing.T) {
	tests := []struct {
		in     string
		expRes []HabPkg
		expErr error
	}{
		{
			"acme/pianos\nacme/fiddle",
			[]HabPkg{New("acme", "pianos"), New("acme", "fiddle")},
			nil,
		},
		{
			"acme/pianos\n\n\n\n\n\n\n\n\n\nacme/fiddle",
			[]HabPkg{New("acme", "pianos"), New("acme", "fiddle")},
			nil,
		},
		{
			"acme/pianos          \nacme/fiddle",
			[]HabPkg{New("acme", "pianos"), New("acme", "fiddle")},
			nil,
		},
		{
			"acme/pianos\n\n\n\n\n\n\n\n\n\nnosep",
			nil,
			errors.New("invalid package identifier 'nosep'"),
		},
	}

	for _, tt := range tests {
		sp, err := FromStrings(tt.in)
		if tt.expErr != nil {
			assert.EqualError(t, err, tt.expErr.Error(), tt.expErr.Error())
		} else {
			assert.Equal(t, tt.expRes, sp)
		}
	}
}

func TestServicePathAccessors(t *testing.T) {
	sp, _ := FromString("acme/pianos")
	assert.Equal(t, "acme", sp.Origin())
	assert.Equal(t, "pianos", sp.Name())
}

func TestMarshaller(t *testing.T) {
	tests := []struct {
		in     string
		expRes HabPkg
		expErr error
	}{
		{
			"acme/pianos",
			HabPkg{"acme", "pianos", "", ""},
			nil,
		},
		{
			"acme/pianos/1.0",
			HabPkg{"acme", "pianos", "1.0", ""},
			nil,
		},
		{
			"acme/pianos/1.0/20180207070000",
			HabPkg{"acme", "pianos", "1.0", "20180207070000"},
			nil,
		},
		{
			"nosep",
			HabPkg{},
			errors.New("invalid package identifier 'nosep'"),
		},
		{
			"there/are/too/many/seps",
			HabPkg{},
			errors.New("invalid package identifier 'there/are/too/many/seps'"),
		},
	}

	for _, tt := range tests {
		sp := HabPkg{}
		err := sp.UnmarshalText([]byte(tt.in))
		assert.Equal(t, tt.expRes, sp)
		if tt.expErr != nil {
			assert.EqualError(t, err, tt.expErr.Error(), tt.expErr.Error())
		} else {
			txt, err := sp.MarshalText()
			require.NoError(t, err)
			assert.Equal(t, tt.in, string(txt))
		}
	}
}
