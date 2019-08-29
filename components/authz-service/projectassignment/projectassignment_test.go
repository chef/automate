package projectassignment

import (
	"math/rand"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCalculateProjectDiff(t *testing.T) {
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"there are only new projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b"}, CalculateProjectDiff([]string{}, []string{"a", "b"}))
		}},
		{"there are only old projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b"}, CalculateProjectDiff([]string{"a", "b"}, []string{}))
		}},
		{"one project is removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a"}, CalculateProjectDiff([]string{"a", "b"}, []string{"b"}))
		}},
		{"multiple projects are removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c"}, CalculateProjectDiff([]string{"a", "b", "c"}, []string{"b"}))
		}},
		{"one project is added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"c"}, CalculateProjectDiff([]string{"a", "b"}, []string{"a", "b", "c"}))
		}},
		{"multiple projects are added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "d"}, CalculateProjectDiff([]string{"b", "c"}, []string{"a", "b", "c", "d"}))
		}},
		{"multiple projects are added and removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c", "d"}, CalculateProjectDiff([]string{"b", "c"}, []string{"a", "b", "d"}))
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
	}
}
