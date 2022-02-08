package integration_test

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestStorageStagedDataLoadSampleData(t *testing.T) {

	t.Run("Store the staged data from migrations.json", func(t *testing.T) {

		jsonFile, err := os.Open("./migrations.json")
		assert.NoError(t, err)

		// Read data from migrations.json
		byteValue, err := ioutil.ReadAll(jsonFile)
		assert.NoError(t, err)

		err = infraProxyMigration.StoreStagedData(ctx, "1234", byteValue)
		assert.NoError(t, err)

	})
}
