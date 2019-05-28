package main

import (
	"bytes"
	"encoding/json"
	"io"
	"io/ioutil"
	"os"
	"strings"
	"text/template"

	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform"
)

type Opts struct {
	Debug      bool
	ConfigPath string
}

var opts = Opts{}

func main() {
	err := newCmd().Execute()
	if err != nil {
		logrus.Fatal(err)
	}
}

func newCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "render-template TEMPLATE_NAME OUTPUT_PATH",
		Short: "Renders a template for the current service",
		RunE:  renderTemplate,
		Args:  cobra.RangeArgs(1, 2),
	}

	cmd.PersistentFlags().BoolVarP(
		&opts.Debug,
		"debug",
		"d",
		false,
		"Enabled debug output")

	cmd.PersistentFlags().StringVar(
		&opts.ConfigPath,
		"conf",
		"",
		"The path to the user config. Must be json.")

	return cmd
}

type UserData map[string]interface{}

type TemplateData struct {
	Platform *platform.Config
	Cfg      UserData
}

func renderTemplate(cmd *cobra.Command, args []string) error {
	if opts.Debug {
		logrus.SetLevel(logrus.DebugLevel)
	} else {
		logrus.SetLevel(logrus.InfoLevel)
	}

	platformConfig, err := platform.ConfigFromEnvironment()
	if err != nil {
		return err
	}

	var userData UserData
	if opts.ConfigPath != "" {
		confData, err := ioutil.ReadFile(opts.ConfigPath)
		if err != nil {
			return err
		}

		if err := json.Unmarshal(confData, &userData); err != nil {
			return err
		}
	}

	templateData := TemplateData{
		Platform: platformConfig,
		Cfg:      userData,
	}

	funcs := template.FuncMap{
		"cfg": func() UserData {
			return userData
		},
	}

	t, err := platformConfig.LoadTemplates(funcs)
	if err != nil {
		return err
	}

	buffer := &bytes.Buffer{}
	if err := t.ExecuteTemplate(buffer, args[0], templateData); err != nil {
		return err
	}

	var out fileutils.WriteCloserFailer = stdoutWriteCloserFailer{os.Stdout}
	if len(args) == 2 {
		out, err = fileutils.NewAtomicWriter(args[1], fileutils.WithAtomicWriteFileMode(0600))
		if err != nil {
			return err
		}
		defer out.Close() // nolint: errcheck
	}

	// (jaym)
	// You think i'm crazy. Well, I'm not. This is the only sane way.
	// golang's templating gives you 3 tunables about what to do when a value
	// is missing: error, replace with a zero-value, or ignore
	// error: It seems this mode fails even if you try to check for the existence
	//        of a missing key, so it's not usable
	// zero-value: Seems sane, but it doesn't work when you have a nested nil struct.
	//             For example, if your data was {} and you tried to check {{if .Foo.Bar}}
	//             you'd get an error. If you just rendered {{.Foo}}, you'd get <no value>
	//             because https://github.com/golang/go/issues/24963
	// ignore: errors are allowed in this case. We're free to check {{if .Foo.Bar.Baz}} and
	//         as one would expect, it is false.
	//         The problem here is that if you rendered {{.Foo.Bar.Baz}}, golang is nice enough
	//         to render "<no value>". There's no way to do anything else, like as it to tell
	//         us or just render an empty string.
	// The error mode doesn't work for us at all. The remaining 2 require us to deal with <no value>.
	// I initially thought we could do this by checking fields that could be empty, but this didn't
	// work so well as I ended up making incorrect assumptions about what could or could not be empty.
	// So, we're going to make the templates behave like hab's templates when a value is missing. If
	// you render it, we'll put in an empty string.
	// If anyone want's to actually use <no value> as a valid value, sorry. It won't work.
	s := strings.ReplaceAll(buffer.String(), "<no value>", "")
	if _, err := io.WriteString(out, s); err != nil {
		return err
	}

	return out.Close()
}

type stdoutWriteCloserFailer struct {
	*os.File
}

func (stdoutWriteCloserFailer) Fail(err error) error {
	return err
}
