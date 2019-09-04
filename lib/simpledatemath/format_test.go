package simpledatemath

import (
	"fmt"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func parseValidExpression(t *testing.T, expression string, expected time.Duration) {
	t.Run(fmt.Sprintf("expression %q should evaluate to %q", expression, expected), func(t *testing.T) {
		actual, err := Parse(expression)
		require.NoError(t, err)
		assert.Equal(t, expected, actual)
	})
}

func parseInvalidExpression(t *testing.T, expression, expectedErrMsg string) {
	t.Run(fmt.Sprintf("expression %q should return an err with message %q", expression, expectedErrMsg), func(t *testing.T) {
		_, err := Parse(expression)
		require.Error(t, err)
		assert.Equal(t, expectedErrMsg, err.Error())
	})
}

func TestParseDateMath(t *testing.T) {
	parseValidExpression(t, "1d", 24*time.Hour)
	parseValidExpression(t, "30d", 30*24*time.Hour)
	parseValidExpression(t, "9h", 9*time.Hour)
	parseValidExpression(t, "90H", 90*time.Hour)
	parseValidExpression(t, "1m", time.Minute)
	parseValidExpression(t, "86400s", 86400*time.Second)
	parseValidExpression(t, "0h", 0)

	parseInvalidExpression(t, "", `simpledatemath: invalid duration expression ""`)
	parseInvalidExpression(t, "1", `simpledatemath: duration expression "1" does not have a unit`)
	parseInvalidExpression(t, "h", `simpledatemath: duration expression "h" does not start with a number`)
	parseInvalidExpression(t, "1.5h", `simpledatemath: duration expression containing decimal fractions is not supported "1.5h"`)
	parseInvalidExpression(t, "4z", `simpledatemath: duration expression "4z" has an invalid or unsupported unit "z"`)
	parseInvalidExpression(t, "-1d", `simpledatemath: duration expression "-1d" does not start with a number`)
	parseInvalidExpression(t, "+1d", `simpledatemath: duration expression "+1d" does not start with a number`)
	parseInvalidExpression(t, "100000000000000000000h", `simpledatemath: duration expression integer is too large "100000000000000000000h"`)
	parseInvalidExpression(t, "123456789!$/(", `simpledatemath: duration expression "123456789!$/(" has an invalid or unsupported unit "!$/("`)
	parseInvalidExpression(t, "40mm", `simpledatemath: duration expression "40mm" has an invalid or unsupported unit "mm"`)
	// 	{
	// 		value:   "1.5d",
	// 		withErr: "invalid time unit: '1.5d'",
	// 	},

	// for _, test := range cases {
	// 	t.Run(, func(t *testing.T) {
	// 		result := validateDateMath(test.value)
	// 		if test.shouldErr {
	// 			if assert.NotNil(t, result) {
	// 				assert.Equal(t, test.withErr, result.Error())
	// 			}
	// 		} else {
	// 			assert.Nil(t, result)
	// 		}
	// 	})
	// }

}
