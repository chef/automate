package grpctest

import (
	"bufio"
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"testing"

	"github.com/golang/protobuf/proto"
	plugin "github.com/golang/protobuf/protoc-gen-go/plugin"
	"github.com/jhump/protoreflect/desc"
	"github.com/stretchr/testify/require"
)

// GenerateCode runs a PGS-based code generator (in memory), and returns its
// response (*plugin.CodeGeneratorResponse)
func GenerateCode(t *testing.T,
	pluginName string,
	fds []*desc.FileDescriptor) (*plugin.CodeGeneratorResponse, func(), error) {

	root, err := rootDir()
	if err != nil {
		return nil, nil, err
	}

	var pluginPackage string
	switch pluginName {
	case "policy":
		pluginPackage = filepath.Join(root, "components/automate-grpc/protoc-gen-policy")
	default:
		return nil, nil, fmt.Errorf("unknown plugin %q", pluginName)
	}

	filesToGen := make([]string, len(fds))
	for i, fd := range fds {
		filesToGen[i] = fd.GetName()
	}
	inputReq := plugin.CodeGeneratorRequest{
		FileToGenerate: filesToGen,
		ProtoFile:      ConvertToFDPs(fds),
	}
	inp, err := proto.Marshal(proto.Message(&inputReq))
	if err != nil {
		t.Fatalf("marshal CodeGeneratorRequest %v: %s", inputReq, err)
	}
	inbuf := bytes.NewReader(inp)
	output := bytes.Buffer{}
	outbuf := bufio.NewWriter(&output)

	err = outbuf.Flush()
	if err != nil {
		t.Fatalf("error flushing output buffer: %s", err)
	}

	// First, we build a throwaway binary, then we execute, feeding the input,
	// and capturing the output, in the same way protoc would do it.
	h, err := buildExecutable(pluginName, pluginPackage)
	require.NoError(t, err, "failed to create temp executable")

	cmd := exec.Command(h.binPath())
	cmd.Stdin = inbuf
	cmd.Stdout = outbuf
	err = cmd.Run()
	if e, ok := err.(*exec.ExitError); ok && !e.Success() {
		return nil, h.cleanup, e
	}

	// read response from generator
	resp := plugin.CodeGeneratorResponse{}
	err = proto.Unmarshal(output.Bytes(), &resp)
	require.NoError(t, err, "error in response unmarshalling")
	return &resp, h.cleanup, nil
}

type binHelper struct {
	tmpDir, name string
}

func (h binHelper) cleanup() {
	_ = os.RemoveAll(h.tmpDir)
}

func (h binHelper) binPath() string {
	return filepath.Join(h.tmpDir, h.name)
}

// This is for making the tests not too slow -- it'll cause the `go build` call
// in GenerateCode to not rebuild all the time (go build is clever here, but it
// requires the output name to be the same).
// Note: this is not safe for multiple plugins -- but we're only testing a
// single one right now anyways.
var (
	binCached   binHelper
	errCached   error
	buildBinary sync.Once
)

func buildExecutable(pluginName, mainFile string) (binHelper, error) {
	buildBinary.Do(func() {
		h := binHelper{name: pluginName}
		tmpDir, err := ioutil.TempDir("", pluginName)
		if err != nil {
			errCached = err
			return
		}
		h.tmpDir = tmpDir
		cmd := exec.Command("go", "build", "-o", h.binPath(), mainFile)
		cmd.Stderr = os.Stderr
		cmd.Stdout = os.Stderr
		if err := cmd.Run(); err != nil {
			errCached = err
			return
		}
		binCached = h
	})

	return binCached, errCached
}
