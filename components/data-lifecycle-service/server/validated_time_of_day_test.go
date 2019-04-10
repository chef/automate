package server

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDefaultTimeOfDay(t *testing.T) {
	tod := DefaultTimeOfDay()
	assert.Equal(t, 0, tod.Hour())
	assert.Equal(t, 0, tod.Min())
	assert.Equal(t, 0, tod.Sec())
}

func TestValidString1(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("00:00:00"))
	require.NoError(t, err)
	assert.Equal(t, 0, tod.Hour())
	assert.Equal(t, 0, tod.Min())
	assert.Equal(t, 0, tod.Sec())
}

func TestValidString2(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("23:59:49"))
	require.NoError(t, err)
	assert.Equal(t, 23, tod.Hour())
	assert.Equal(t, 59, tod.Min())
	assert.Equal(t, 49, tod.Sec())
}

func TestInvalidString(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("12:00"))
	assert.Error(t, err)
}

func TestInvalidHour(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("24:00:00"))
	assert.EqualError(t, err, "Invalid hour value 24")
}

func TestInvalidHourNegative(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("-1:00:00"))
	assert.EqualError(t, err, "Invalid hour value -1")
}

func TestInvalidMin(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("00:60:00"))
	assert.EqualError(t, err, "Invalid minute value 60")
}

func TestInvalidMinNegative(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("00:-1:00"))
	assert.EqualError(t, err, "Invalid minute value -1")
}

func TestInvalidSec(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("00:00:60"))
	assert.EqualError(t, err, "Invalid second value 60")
}

func TestInvalidSecNegative(t *testing.T) {
	tod := ValidatedTimeOfDay{}
	err := tod.UnmarshalText([]byte("00:00:-1"))
	assert.EqualError(t, err, "Invalid second value -1")
}
