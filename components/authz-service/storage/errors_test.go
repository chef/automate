package storage_test

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/authz-service/storage"
)

func TestNewTxCommitError(t *testing.T) {
	t.Run("TxCommitError.Error() does not cause an infinite loop", func(t *testing.T) {
		errStr := "this is some transaction error"
		err := errors.New(errStr)
		txErr := storage.NewTxCommitError(err)
		assert.Equal(t, "commit db transaction: "+errStr, txErr.Error())
	})
}
