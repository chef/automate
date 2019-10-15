package projectassignment

import (
	"math/rand"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCalculateProjectDiffForUpdate(t *testing.T) {
	isUpdate := true
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"unassigned to unassigned", func(t *testing.T) {
			assert.ElementsMatch(t, []string{}, CalculateProjectDiff([]string{}, []string{}, isUpdate))
		}},
		{"there are only new projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b", "(unassigned)"}, CalculateProjectDiff([]string{}, []string{"a", "b"}, isUpdate))
		}},
		{"no projects were removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{}, CalculateProjectDiff([]string{"a", "b"}, []string{"a", "b"}, isUpdate))
		}},
		{"there are only old projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b", "(unassigned)"}, CalculateProjectDiff([]string{"a", "b"}, []string{}, isUpdate))
		}},
		{"one project is removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a"}, CalculateProjectDiff([]string{"a", "b"}, []string{"b"}, isUpdate))
		}},
		{"multiple projects are removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c"}, CalculateProjectDiff([]string{"a", "b", "c"}, []string{"b"}, isUpdate))
		}},
		{"one project is added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"c"}, CalculateProjectDiff([]string{"a", "b"}, []string{"a", "b", "c"}, isUpdate))
		}},
		{"multiple projects are added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "d"}, CalculateProjectDiff([]string{"b", "c"}, []string{"a", "b", "c", "d"}, isUpdate))
		}},
		{"multiple projects are added and removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c", "d"}, CalculateProjectDiff([]string{"b", "c"}, []string{"a", "b", "d"}, isUpdate))
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
	}
}

func TestCalculateProjectDiffForCreate(t *testing.T) {
	isUpdate := false
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"unassigned to unassigned", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"(unassigned)"}, CalculateProjectDiff([]string{}, []string{}, isUpdate))
		}},
		{"there are only new projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b", "(unassigned)"}, CalculateProjectDiff([]string{}, []string{"a", "b"}, isUpdate))
		}},
		{"no projects were removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{}, CalculateProjectDiff([]string{"a", "b"}, []string{"a", "b"}, isUpdate))
		}},
		{"there are only old projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b", "(unassigned)"}, CalculateProjectDiff([]string{"a", "b"}, []string{}, isUpdate))
		}},
		{"one project is removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a"}, CalculateProjectDiff([]string{"a", "b"}, []string{"b"}, isUpdate))
		}},
		{"multiple projects are removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c"}, CalculateProjectDiff([]string{"a", "b", "c"}, []string{"b"}, isUpdate))
		}},
		{"one project is added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"c"}, CalculateProjectDiff([]string{"a", "b"}, []string{"a", "b", "c"}, isUpdate))
		}},
		{"multiple projects are added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "d"}, CalculateProjectDiff([]string{"b", "c"}, []string{"a", "b", "c", "d"}, isUpdate))
		}},
		{"multiple projects are added and removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c", "d"}, CalculateProjectDiff([]string{"b", "c"}, []string{"a", "b", "d"}, isUpdate))
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
	}
}
