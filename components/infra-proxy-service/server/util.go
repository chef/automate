package server

import (
	"fmt"
	"reflect"
	"strconv"
)

// SafeStringFromMap returns the value referenced by `key` in `values`. If value is nil,
// it returns an empty string; otherwise it returns the original string.
func SafeStringFromMap(values map[string]interface{}, key string) string {
	if values[key] == nil {
		return ""
	}
	return values[key].(string)
}

// SafeStringFromMapFloat returns the value referenced by `key` in `values`. If value is nil,
// it returns an empty string; otherwise it returns the base 64 float string.
func SafeStringFromMapFloat(values map[string]interface{}, key string) string {
	if values[key] == nil {
		return ""
	}
	return strconv.FormatFloat(values[key].(float64), 'E', -1, 64)
}

// SafeSliceFromMap returns the value referenced by `key` in `values`. If value is nil,
// it returns an empty slice string; otherwise it returns the original slice string.
func SafeSliceFromMap(values map[string]interface{}, key string) []string {
	value := reflect.ValueOf(values[key])
	switch value.Kind() {
	case reflect.Slice:
		t := make([]string, value.Len())
		for i := 0; i < value.Len(); i++ {
			t[i] = fmt.Sprint(value.Index(i))
		}
		return t
	}

	return []string{}
}
