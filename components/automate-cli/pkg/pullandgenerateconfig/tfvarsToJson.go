package pullandgenerateconfig

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"strings"

	"github.com/hashicorp/hcl/v2"
	"github.com/hashicorp/hcl/v2/hclsyntax"
)

func ConvTfvarToJson(filename string) (string, error) {
	if len(strings.TrimSpace(filename)) < 1 {
		return "", fmt.Errorf("Invalid or empty argument")
	}

	src, err := ioutil.ReadFile(filename)
	if err != nil {
		return "", fmt.Errorf("Error reading %q: %s", filename, err)
	}

	f, diags := hclsyntax.ParseConfig(src, filename, hcl.Pos{Line: 1, Column: 1})
	if diags.HasErrors() {
		return "", fmt.Errorf("Error parsing %q: %s", filename, diags.Error())
	}
	if f.Body == nil {
		return "", fmt.Errorf("{}")
	}

	attrs, diags := f.Body.JustAttributes()
	if diags.HasErrors() {
		return "", fmt.Errorf("Error evaluating %q: %s", filename, diags.Error())
	}

	values := make(map[string]json.RawMessage, len(attrs))
	for name, attr := range attrs {
		value, diags := attr.Expr.Value(&hcl.EvalContext{})
		if diags.HasErrors() {
			return "", fmt.Errorf("Error evaluating %q in %q: %s", name, filename, diags.Error())
		}
		buf, err := json.Marshal(value)
		if err != nil {
			return "", fmt.Errorf("Error converting %q in %q to JSON: %s", name, filename, err)
		}
		values[name] = json.RawMessage(buf)
	}

	output, err := json.MarshalIndent(values, "", "  ")
	if err != nil {
		return "", fmt.Errorf("Error marshalling %q to JSON: %s", filename, err)
	}
	return string(output), nil
}
