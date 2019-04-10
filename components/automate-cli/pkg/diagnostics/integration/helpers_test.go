package integration

import (
	"testing"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func TestRetry(t *testing.T) {
	t.Run("runs the correct number of times", func(t *testing.T) {
		count := 0
		Retry(3, 0, func() error {
			count++
			return errors.New("error")
		})
		assert.Equal(t, 3, count)
	})
}
