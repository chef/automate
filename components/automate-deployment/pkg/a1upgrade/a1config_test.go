package a1upgrade

import (
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestLoadDeliveryRunning(t *testing.T) {
	config := NewA1Config()
	config.DeliveryRunningPath = fixturePath("delivery-running.json")
	err := config.LoadDeliveryRunning()

	assert.Equal(t, err, nil)
	assert.Equal(t, config.DeliveryRunning.Delivery.FQDN, "a1-migration.test")
}

func TestLoadDeliveryRunningWithStrings(t *testing.T) {
	config := NewA1Config()
	config.DeliveryRunningPath = fixturePath("delivery-running-with-strings.json")
	err := config.LoadDeliveryRunning()
	require.NoError(t, err)
	port, err := config.DeliveryRunning.Delivery.PostgreSQL.Port.Int64()
	require.NoError(t, err)
	assert.Equal(t, int64(5432), port)
}

func TestLoadDeliverySecrets(t *testing.T) {
	config := NewA1Config()
	config.DeliverySecretsPath = fixturePath("delivery-secrets.json")
	err := config.LoadDeliverySecrets()

	assert.Equal(t, err, nil)
	assert.Equal(
		t,
		config.DeliverySecrets.Postgresql.SuperuserPassword,
		"6591266f724f79da0ef8a984dee897b51eba969df9345ce0ddffdd9af627d39a83b3deb804ac4f2c000473c703a970d5f692",
	)
}

func TestNoUseOfOutlawedTypes(t *testing.T) {
	config := NewA1Config()
	noOutlawedTypes(t, "", reflect.ValueOf(config))
}

func noOutlawedTypes(t *testing.T, name string, v reflect.Value) {
	switch v.Kind() {
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		fallthrough
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		fallthrough
	case reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128:
		explanation := `
Any numeric elements of the A1 configuration must use the json.Number
type. The lax validation of the A1 configuration means that in many
cases users can specify a string in place of a numeric value without
issue. To ensure we can handle such configurations, we use
json.Number everywhere.

If you have a really good reason for not using json.Number, I trust
you, either add an exception into this test or remove this test. Your
call my friend.

`
		msg := fmt.Sprintf("It appears that a A1 Configuration struct contains a numeric field\n\nField: %s\nType: %v\n%s", name, v.Kind(), explanation)
		assert.Fail(t, msg)
	case reflect.Ptr:
		noOutlawedTypes(t, name, v.Elem())
	case reflect.Struct:
		for i := 0; i < v.NumField(); i++ {
			noOutlawedTypes(t, v.Type().Field(i).Name, v.Field(i))
		}
	}
}

func fixturePath(f string) string {
	wd, _ := os.Getwd()
	return filepath.Join(wd, "testdata/", f)
}
