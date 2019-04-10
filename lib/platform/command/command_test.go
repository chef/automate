package command_test

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/platform/command"
)

func TestStdout(t *testing.T) {
	exec := command.NewExecExecutor()

	t.Run("Run redirects output to given writer when given Stdout option", func(t *testing.T) {
		output := new(bytes.Buffer)
		err := exec.Run("echo", command.Args("test-value-1"), command.Stdout(output))
		require.NoError(t, err)
		assert.Equal(t, "test-value-1\n", output.String())
	})

	t.Run("Start redirects output to given writer when given Stdout option", func(t *testing.T) {
		output := new(bytes.Buffer)
		waitFunc, err := exec.Start("echo", command.Args("test-value-1"), command.Stdout(output))
		require.NoError(t, err)
		err = waitFunc()
		require.NoError(t, err)
		assert.Equal(t, "test-value-1\n", output.String())
	})

	t.Run("Output returns error if you try to set stdout", func(t *testing.T) {
		_, err := exec.Output("true", command.Stdout(new(bytes.Buffer)))
		assert.Error(t, err)
	})

	t.Run("CombinedOutput returns error if you try to set stdout", func(t *testing.T) {
		_, err := exec.CombinedOutput("true", command.Stdout(new(bytes.Buffer)))
		assert.Error(t, err)
	})
}

func TestStderr(t *testing.T) {
	exec := command.NewExecExecutor()

	t.Run("Run redirects error output to given writer when given Stderr option", func(t *testing.T) {
		output := new(bytes.Buffer)
		err := exec.Run("sh", command.Args("-c", "echo test-value-1 1>&2"), command.Stderr(output))
		require.NoError(t, err)
		assert.Equal(t, "test-value-1\n", output.String())
	})

	t.Run("Run returns error but Stderr isn't populated when Stderr option was given", func(t *testing.T) {
		output := new(bytes.Buffer)
		err := exec.Run("sh", command.Args("-c", "echo test-value-1 1>&2; exit 1"), command.Stderr(output))
		require.Error(t, err)
		assert.Equal(t, "test-value-1\n", output.String())
		assert.Equal(t, "(no stderr available)", string(command.StderrFromError(err)))
	})

	t.Run("Start redirects error output to given writer when given Stderr option", func(t *testing.T) {
		output := new(bytes.Buffer)
		waitFunc, err := exec.Start("sh", command.Args("-c", "echo test-value-1 1>&2"), command.Stderr(output))
		require.NoError(t, err)
		err = waitFunc()
		require.NoError(t, err)
		assert.Equal(t, "test-value-1\n", output.String())
	})

	t.Run("Start returns error but Stderr isn't populated when Stderr option was given", func(t *testing.T) {
		output := new(bytes.Buffer)
		waitFunc, err := exec.Start("sh", command.Args("-c", "echo test-value-1 1>&2; exit 1"), command.Stderr(output))
		require.NoError(t, err)
		err = waitFunc()
		require.Error(t, err)
		assert.Equal(t, "test-value-1\n", output.String())
		assert.Equal(t, "(no stderr available)", string(command.StderrFromError(err)))
	})

	t.Run("CombinedOutput returns error if you try to set stderr", func(t *testing.T) {
		_, err := exec.CombinedOutput("true", command.Stderr(new(bytes.Buffer)))
		assert.Error(t, err)
	})
}

func TestPipeTo(t *testing.T) {
	exec := command.NewExecExecutor()
	t.Run("Run redirects output to given file when given PipeTo option", func(t *testing.T) {
		file, err := ioutil.TempFile("", "pipe-from-test")
		require.NoError(t, err)
		defer os.Remove(file.Name())
		file.Close()
		err = exec.Run("echo", command.Args("test-value-1"), command.PipeTo(file.Name()))
		require.NoError(t, err)
		output, err := ioutil.ReadFile(file.Name())
		require.NoError(t, err)
		assert.Equal(t, "test-value-1\n", string(output))
	})

	t.Run("Start redirects output to given file when given PipeTo option", func(t *testing.T) {
		file, err := ioutil.TempFile("", "pipe-from-test")
		require.NoError(t, err)
		defer os.Remove(file.Name())
		file.Close()
		waitFunc, err := exec.Start("echo", command.Args("test-value-1"), command.PipeTo(file.Name()))
		require.NoError(t, err)
		err = waitFunc()
		require.NoError(t, err)
		output, err := ioutil.ReadFile(file.Name())
		require.NoError(t, err)
		assert.Equal(t, "test-value-1\n", string(output))
	})

	t.Run("Output returns error if you try to set stdout", func(t *testing.T) {
		_, err := exec.Output("true", command.PipeTo("/dev/null"))
		assert.Error(t, err)
	})

	t.Run("CombinedOutput returns error if you try to set stdout", func(t *testing.T) {
		_, err := exec.CombinedOutput("true", command.PipeTo("/dev/null"))
		assert.Error(t, err)
	})
}

func TestErrorsAndOutput(t *testing.T) {
	exec := command.NewExecExecutor()
	t.Run("Run makes error output available in returned error when the command fails", func(t *testing.T) {
		err := exec.Run("bash", command.Args("-c", "echo 'error output' 1>&2; exit 1"))
		require.Error(t, err)
		assert.Equal(t, "error output\n", string(command.StderrFromError(err)))
	})
	t.Run("Start makes error output available in returned error when the command fails", func(t *testing.T) {
		waitFunc, err := exec.Start("bash", command.Args("-c", "echo 'error output' 1>&2; exit 1"))
		require.NoError(t, err)
		err = waitFunc()
		require.Error(t, err)
		assert.Equal(t, "error output\n", string(command.StderrFromError(err)))
	})
	t.Run("Output makes error output available in returned error when the command fails", func(t *testing.T) {
		output, err := exec.Output("bash", command.Args("-c", "echo 'stdout put'; echo 'error output' 1>&2; exit 1"))
		require.Error(t, err)
		assert.Equal(t, "stdout put\n", output)
		assert.Equal(t, "error output\n", string(command.StderrFromError(err)))
	})
	t.Run("CombinedOutput has error in returned string, which can be read even if the command errored", func(t *testing.T) {
		output, err := exec.CombinedOutput("bash", command.Args("-c", "echo 'stdout put'; echo 'error output' 1>&2; exit 1"))
		require.Error(t, err)
		assert.Equal(t, "stdout put\nerror output\n", output)
	})
	t.Run("Output returns safe value to read even when command failed with no output", func(t *testing.T) {
		output, err := exec.CombinedOutput("bash", command.Args("-c", "exit 1"))
		require.Error(t, err)
		assert.Equal(t, "", output)
	})
	t.Run("Output returns safe value to read even when command tries to set stdout", func(t *testing.T) {
		output, err := exec.CombinedOutput("true", command.Stdout(new(bytes.Buffer)))
		require.Error(t, err)
		assert.Equal(t, "", output)
	})
	t.Run("CombinedOutput returns safe value to read even when command tries to set stdout", func(t *testing.T) {
		output, err := exec.CombinedOutput("true", command.Stdout(new(bytes.Buffer)))
		require.Error(t, err)
		assert.Equal(t, "", output)
	})
}

func TestConvenienceFunctions(t *testing.T) {
	// The underlying behaviors are already well tested, so here
	// we really just want to make sure the functions exist.
	t.Run("Run runs a commmad", func(t *testing.T) {
		err := command.Run("true")
		assert.NoError(t, err)
	})
	t.Run("Start runs a commmad", func(t *testing.T) {
		waitFunc, err := command.Start("true")
		assert.NoError(t, err)
		err = waitFunc()
		require.NoError(t, err)
	})
	t.Run("Output runs a commmad", func(t *testing.T) {
		output, err := command.Output("echo", command.Args("foo"))
		assert.NoError(t, err)
		assert.Equal(t, "foo\n", output)
	})
	t.Run("CombinedOutput runs a commmad", func(t *testing.T) {
		output, err := command.CombinedOutput("bash", command.Args("-c", "echo foo 1>&2; echo bar"))
		assert.NoError(t, err)
		assert.Equal(t, "foo\nbar\n", output)
	})
}

func TestCommonBehaviors(t *testing.T) {
	exec := command.NewExecExecutor()

	type test struct {
		desc     string
		testFunc func(f func(string, ...command.Opt) error, t *testing.T)
	}

	tests := []test{
		{"errors if you try to use context and timeout",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("sleep",
					command.Args("30"),
					command.Context(context.Background()),
					command.Timeout(1*time.Minute))
				assert.Error(t, err)

				err = f("sleep",
					command.Args("30"),
					command.Timeout(1*time.Minute),
					command.Context(context.Background()))
				assert.Error(t, err)
			},
		},
		{"errors if you try to use PipeFrom and Stdin",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("true",
					command.PipeFrom("/dev/zero"),
					command.Stdin(strings.NewReader("")))
				assert.Error(t, err)

				err = f("true",
					command.Stdin(strings.NewReader("")),
					command.PipeFrom("/dev/zero"))
				assert.Error(t, err)
			},
		},
		{"errors if you try to use Stdin twice",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("true",
					command.Stdin(strings.NewReader("")),
					command.Stdin(strings.NewReader("")))
				assert.Error(t, err)
			},
		},
		{"errors if you try to use Stdout twice",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("true",
					command.Stdout(new(bytes.Buffer)),
					command.Stdout(new(bytes.Buffer)))
				assert.Error(t, err)
			},
		},
		{"errors if you try to use Stderr twice",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("true",
					command.Stderr(new(bytes.Buffer)),
					command.Stderr(new(bytes.Buffer)))
				assert.Error(t, err)
			},
		},
		{"respects the configured context",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				ctx, cancel := context.WithCancel(context.Background())
				cancel()
				err := f("sleep", command.Args("30"), command.Context(ctx))
				assert.Error(t, err)
			},
		},
		{"respects the configured timeout",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("sleep", command.Args("30"), command.Timeout(5*time.Millisecond))
				assert.Error(t, err)
			},
		},
		{"0 timeout means no timeout",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("sleep", command.Args("0.5"), command.Timeout(0*time.Millisecond))
				assert.NoError(t, err)
			},
		},

		{"returns an error if the command exits non-zero",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("false")
				assert.Error(t, err)
			},
		},
		{"returns no error if the command exits zero",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("true")
				assert.NoError(t, err)
			},
		},
		{"returns no error if the command exits zero",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("true")
				assert.NoError(t, err)
			},
		},
		{"sets input of command to file given in PipeFrom argument",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("grep", command.Args("foo"), command.Stdin(strings.NewReader("bar\nfoo\nbaz")))
				assert.NoError(t, err)
			},
		},
		{"sets input of command to file given in PipeFrom argument",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				file, err := ioutil.TempFile("", "pipe-from-test")
				require.NoError(t, err)
				defer os.Remove(file.Name())
				io.WriteString(file, "bar\nfoo\nbaz\n")
				file.Sync()
				file.Close()
				err = f("grep", command.Args("foo"), command.PipeFrom(file.Name()))
				assert.NoError(t, err)
			},
		},
		{"returns an error if a PipeFrom file can't be opened",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("grep", command.Args("foo"), command.PipeFrom("/definitely/should/not/exist"))
				assert.Error(t, err)
			},
		},
		{"sets environment variable given in the options",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				err := f("bash", command.Args("-c", "env | grep TEST_VAR_WOMBAT"), command.Envvar("TEST_VAR_WOMBAT", "test-value-wombat"))
				assert.NoError(t, err)
			},
		},
		{"appends to the current process environment",
			func(f func(string, ...command.Opt) error, t *testing.T) {
				os.Setenv("TEST_VAR_WOMBAT0", "test-value-wombat0")
				err := f("bash", command.Args("-c", "env | grep TEST_VAR_WOMBAT0"), command.Envvar("TEST_VAR_WOMBAT", "test-value-wombat"))
				assert.NoError(t, err)
				os.Unsetenv("TEST_VAR_WOMBAT0")
			},
		},
	}

	for _, test := range tests {
		t.Run(fmt.Sprintf("Run %s", test.desc), func(t *testing.T) {
			test.testFunc(exec.Run, t)
		})
		t.Run(fmt.Sprintf("Start %s", test.desc), func(t *testing.T) {
			test.testFunc(func(cmd string, opts ...command.Opt) error {
				waitFunc, err := exec.Start(cmd, opts...)
				if err != nil {
					return err
				}
				return waitFunc()
			}, t)
		})
		t.Run(fmt.Sprintf("Output %s", test.desc), func(t *testing.T) {
			test.testFunc(func(cmd string, opts ...command.Opt) error {
				_, err := exec.Output(cmd, opts...)
				return err
			}, t)
		})
		t.Run(fmt.Sprintf("CombinedOutput %s", test.desc), func(t *testing.T) {
			test.testFunc(func(cmd string, opts ...command.Opt) error {
				_, err := exec.CombinedOutput(cmd, opts...)
				return err
			}, t)
		})
	}
}
