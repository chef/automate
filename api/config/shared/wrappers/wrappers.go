package wrappers

import "github.com/golang/protobuf/ptypes/wrappers"

// This package provides helper functions to create protobuf wrapper types

// String returns a wrapped string value
func String(s string) *wrappers.StringValue {
	return &wrappers.StringValue{
		Value: s,
	}
}

// Double returns a wrapped double value
func Double(d float64) *wrappers.DoubleValue {
	return &wrappers.DoubleValue{
		Value: d,
	}
}

// Float returns a wrapped float value
func Float(f float32) *wrappers.FloatValue {
	return &wrappers.FloatValue{
		Value: f,
	}
}

// Int32 returns a wrapped int32 value
func Int32(i int32) *wrappers.Int32Value {
	return &wrappers.Int32Value{
		Value: i,
	}
}

// UInt32 returns a wrapped uint32 value
func UInt32(i uint32) *wrappers.UInt32Value {
	return &wrappers.UInt32Value{
		Value: i,
	}
}

// Int64 returns a wrapped int64 value
func Int64(i int64) *wrappers.Int64Value {
	return &wrappers.Int64Value{
		Value: i,
	}
}

// UInt64 returns a wrapped uint64 value
func UInt64(i uint64) *wrappers.UInt64Value {
	return &wrappers.UInt64Value{
		Value: i,
	}
}

// Bool returns a wrapped bool value
func Bool(b bool) *wrappers.BoolValue {
	return &wrappers.BoolValue{
		Value: b,
	}
}

// Bytes returns a wrapped bytes value
func Bytes(b []byte) *wrappers.BytesValue {
	return &wrappers.BytesValue{
		Value: b,
	}
}
