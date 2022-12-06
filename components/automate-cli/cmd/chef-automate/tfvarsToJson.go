package main

import (
	"encoding/json"
	"io/ioutil"
	"strings"

	"github.com/hashicorp/hcl/v2"
	"github.com/hashicorp/hcl/v2/hclsyntax"
	ctyjson "github.com/zclconf/go-cty/cty/json"
)

func convTfvarToJson(filename string) string {
	if len(strings.TrimSpace(filename)) < 1 {
		writer.Printf("Invalid or empty argument")
		return ""
	}
	src, err := ioutil.ReadFile(filename) // nosemgrep
	if err != nil {
		writer.Printf("Error reading %q: %s", filename, err)
		return ""
	}

	f, diags := hclsyntax.ParseConfig(src, filename, hcl.Pos{Line: 1, Column: 1})
	if diags.HasErrors() {
		writer.Printf("Error parsing %q: %s", filename, diags.Error())
		return ""
	}
	if f.Body == nil {
		writer.Println("{}")
		return ""
	}

	attrs, diags := f.Body.JustAttributes()
	if diags.HasErrors() {
		writer.Printf("Error evaluating %q: %s", filename, diags.Error())
		return ""
	}

	values := make(map[string]json.RawMessage, len(attrs))
	for name, attr := range attrs {
		value, diags := attr.Expr.Value(nil)
		if diags.HasErrors() {
			writer.Printf("Error evaluating %q in %q: %s", name, filename, diags.Error())
			return ""
		}
		buf, err := ctyjson.Marshal(value, value.Type())
		if err != nil {
			writer.Printf("Error converting %q in %q to JSON: %s", name, filename, err)
			return ""
		}
		values[name] = json.RawMessage(buf)
	}

	output, err := json.MarshalIndent(values, "", "  ")
	if err != nil {
		writer.Printf("Error marshalling %q to JSON: %s", filename, err)
		return ""
	}
	return string(output)
}
