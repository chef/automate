package es

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestRecommendedHeapSizeGB(t *testing.T) {
	cases := []struct {
		in  int
		out int
	}{
		{134217728, 16}, // clamp max
		{2097152, 1},    // clamp min
		{8164732, 2},    // Round up
		{8548608, 2},    // Round down
		{0, 1},
		{-8548608, 1},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("System memory of %d recommends %d", c.in, c.out), func(t *testing.T) {
			assert.Equal(t, c.out, RecommendedHeapSizeGB(c.in))
		})
	}
}
