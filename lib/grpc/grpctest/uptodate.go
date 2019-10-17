package grpctest

import (
	"bytes"
	"fmt"
	"go/build"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"testing"

	"github.com/golang/protobuf/protoc-gen-go/descriptor"
	"github.com/jhump/protoreflect/desc"
	"github.com/jhump/protoreflect/desc/protoparse"
	"github.com/jhump/protoreflect/desc/protoprint"
	"github.com/pmezard/go-difflib/difflib"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const a2Root = "github.com/chef/automate"

// Note 2018/02/26 (sr): This is done to avoid having to know the level of
// nesting for the code that calls this method, i.e., to avoid having to get
// the number of "../" right.
var (
	goSrc    = filepath.Join(build.Default.GOPATH, "src")
	topLevel = filepath.Join(goSrc, a2Root)
)

// AssertCompiledInUpToDate is an assertion that, given a set of directories
// containing .proto files, such as components/teams-service/teams/, asserts
// that the protobuf descriptor content that is compiled in is in sync with the
// content of the file on disk. To do so, it uses the protoreflect library to
// parse data from both sources, and string-compares the pretty-printed proto3
// source of both.
// Note that this way, whitespace- or comment-only changes do not matter.
func AssertCompiledInUpToDate(t *testing.T, dirs ...string) {
	t.Helper()
	files := findAllProtoFiles(t, topLevel, dirs...)
	assert.NotZero(t, len(files), "expected at least one *.proto file in %v", dirs)

	fds, err := ParseProtoFiles(files)
	if err != nil {
		t.Fatalf("parse proto files %v: %s", files, err)
	}

	for _, fdDisk := range fds {
		t.Run(fdDisk.GetName(), func(t *testing.T) {
			// this loads the compiled-in definitions, which are accessed using their
			// "original" filename
			fdCompiled, err := desc.LoadFileDescriptor(fdDisk.GetName())
			if err != nil {
				// nolint: lll // if google can report "thing not in cache" as "file that exists doesn't exist," I can have a long line.
				t.Fatalf("failed to load compiled-in file %v: %s ('No such file' may indicate you need to import your go package in your test)", fdDisk.GetName(), err)
			}

			assertEqualOrOutputDiff(t, fdDisk.GetName(), toString(t, fdDisk), toString(t, fdCompiled))
		})
	}
}

// AssertAllProtoMethodsAnnotated checks, for each policy version,
// that each public method on each service
// in each proto file is instrumented with a resource and an action.
func AssertAllProtoMethodsAnnotated(t *testing.T, file string, vsnIdentifier string) {
	t.Helper()

	filesToGen := []string{file}
	fds, err := ParseProtoFiles(filesToGen)
	if err != nil {
		t.Fatalf("parse proto file %q: %s", file, err)
	}
	publicRE := regexp.MustCompile(`google.api.http`)
	policyRE := regexp.MustCompile(fmt.Sprintf(`\[%s\]:\<([^>]+)\>`, vsnIdentifier))
	resourceRE := regexp.MustCompile(`resource:"([^"]+)"`)
	actionRE := regexp.MustCompile(`action:"([^"]+)"`)
	for _, svc := range fds[0].GetServices() {
		for _, method := range svc.GetMethods() {
			descriptor := fmt.Sprintf("%s:%s:%s.%s", file, vsnIdentifier, svc.GetName(), method.GetName())
			protoMsg := method.GetOptions()
			if publicRE.MatchString(protoMsg.String()) {
				matches := policyRE.FindStringSubmatch(protoMsg.String())
				if len(matches) < 2 {
					t.Fatalf("%s: no IAM annotations found", descriptor)
				}
				resourceAndAction := matches[1]
				matches = resourceRE.FindStringSubmatch(resourceAndAction)
				if len(matches) < 2 {
					t.Fatalf("%s: no IAM resource found in %q", descriptor, resourceAndAction)
				}
				resource := matches[1]
				matches = actionRE.FindStringSubmatch(resourceAndAction)
				if len(matches) < 2 {
					t.Fatalf("%s: no IAM action found in %q", descriptor, resourceAndAction)
				}
				action := matches[1]
				t.Logf("%s: resource=%s, action=%s\n", descriptor, resource, action)
			}
		}
	}
}

// AssertGeneratedPolicyUpToDate checks that the generated `*.pb.policy.go`
// files corresponding to the proto files with policy-related annotations are
// up to date, i.e., haven't been forgotten to be included in a commit.
// Its argument is a `*.proto` file with policy annotations on its service's
// methods.
func AssertGeneratedPolicyUpToDate(t *testing.T, file string) {
	t.Helper()

	filesToGen := []string{file}
	fds, err := ParseProtoFiles(filesToGen)
	if err != nil {
		t.Fatalf("parse proto file %q: %s", file, err)
	}
	// Note 2018/09/18 (sr): It's currently not save to call the cleanup function
	// returned by GenerateCode in more than one place. (The first call would
	// remove the cached binary, and subsequent tests attempting to use it would
	// fail.) However, we currently don't do that; the PolicyUpToDate tests are
	// run for automate-gateway, the unit tests for protoc-gen-policy are run for
	// components/automate-grpc.
	// If this changes, an attempt to solve this issue was pushed to Github, see
	//
	// https://github.com/chef/automate/commit/a1a53ac378c2cd54f26a62ef85e682f2114d336b
	//
	// But also, since the problem hadn't become apparent, the added complexity
	// didn't seem too appealing to bring in for no pressing reason. There's other
	// ways to solve this problem when we have it, too.
	resp, cleanup, err := GenerateCode(t, "policy", fds)
	require.NoError(t, err)
	defer cleanup()

	for _, generated := range resp.GetFile() {
		onDisk, err := ioutil.ReadFile(filepath.Join(goSrc, generated.GetName()))
		require.NoError(t, err, "read generated file from disk")

		assertEqualOrOutputDiff(t, generated.GetName(), string(onDisk), generated.GetContent())
	}
}

func assertEqualOrOutputDiff(t *testing.T, filename, a, b string) {
	t.Helper()
	diff := difflib.UnifiedDiff{
		A:        difflib.SplitLines(a),
		B:        difflib.SplitLines(b),
		FromFile: filename,
		FromDate: "on disk",
		ToFile:   filename,
		ToDate:   "generated",
		Context:  3,
	}
	text, err := difflib.GetUnifiedDiffString(diff)
	require.NoError(t, err, "error generating diff")
	if text != "" {
		t.Error("expected no difference, got diff:\n" + text)
	}
}

// FindServiceProtos walks the passed directories, and returns the filenames of
// those `.proto` files that contain service definitions.
// It's meant to be used in conjunction with
// grpctest.AssertGeneratedPolicyUpToDate, to not have to enumerate all relevant
// `.proto` files in tests.
func FindServiceProtos(t *testing.T, dirs ...string) []string {
	t.Helper()
	returnedPaths := []string{}

	files := findAllProtoFiles(t, topLevel, dirs...)
	for _, file := range files {
		// read compile-in proto data
		fd, err := desc.LoadFileDescriptor(file)
		require.NoErrorf(t, err, "read compiled-in data for %q", file)

		if len(fd.GetServices()) > 0 {
			returnedPaths = append(returnedPaths, fd.GetName())
		}
	}
	return returnedPaths
}

func toString(t *testing.T, fd *desc.FileDescriptor) string {
	t.Helper()

	printer := protoprint.Printer{}
	buf := bytes.Buffer{}
	err := printer.PrintProtoFile(fd, &buf)
	if err != nil {
		t.Fatalf("print fd %s: %s", fd.GetName(), err)

	}
	return buf.String()
}

func findAllProtoFiles(t *testing.T, base string, dirs ...string) []string {
	t.Helper()

	files := []string{}

	for _, dir := range dirs {
		containing := filepath.Join(base, dir)
		err := filepath.Walk(containing, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				t.Fatalf("access path %q: %v\n", dir, err)
				return err
			}
			if !info.IsDir() && filepath.Ext(info.Name()) == ".proto" {
				rel, err := filepath.Rel(base, path)
				if err != nil {
					t.Fatalf("make %q relative to %q: %s", path, base, err)
				}
				files = append(files, rel)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("error walking dir %q: %s", dir, err)
		}
	}

	return files
}

// ParseProtoFiles takes a slice of string filenames, like
//   "api/interservice/authz/authz.proto"
// attempts to parse them, and returns a slice of *desc.FileDescriptor on
// success.
func ParseProtoFiles(files []string) ([]*desc.FileDescriptor, error) {
	parser := protoparse.Parser{
		ImportPaths: []string{
			// top-level, to reference protos using their component/xyz-service/api/... paths
			topLevel,
			// for google/api/{annotations,http}.proto
			filepath.Join(topLevel, "vendor/github.com/grpc-ecosystem/grpc-gateway/third_party/googleapis/"),
			// for validate/validate.proto
			filepath.Join(topLevel, "vendor/github.com/envoyproxy/protoc-gen-validate/"),
			// for protoc-gen-swagger/options/annotations.proto
			filepath.Join(topLevel, "vendor/github.com/grpc-ecosystem/grpc-gateway/"),
		},
	}
	return parser.ParseFiles(files...)
}

// ConvertToFDPs takes a slice of *desc.FileDescriptor (as returned by either
// protoparse.ParseFiles, or desc.LoadFileDescriptor), and returns the (reverse)
// topologically sorted slice of *descriptor.FileDescriptorProto that contains
// all the dependencies in the right order (i.e., before they're imported), and
// can be used to create a plugin.CodeGeneratorRequest.
func ConvertToFDPs(fds []*desc.FileDescriptor) []*descriptor.FileDescriptorProto {
	collected := []*descriptor.FileDescriptorProto{}
	sorted := topSortDFS(fds)
	for _, fd := range sorted {
		collected = append(collected, fd.AsFileDescriptorProto())
	}

	return collected
}

// What protoc produces in plugin.CodeGeneratorRequest is referred to as
// "topological sort" in various places, and we thus use a well-known algorithm
// for it:
//
// https://en.wikipedia.org/w/index.php?title=Topological_sorting&oldid=805893309#Depth-first_search
//
// Note, however, that instead of the classical definition,
// "[...] for every directed edge uv from vertex u to vertex v, u comes before
// v in the ordering",
// we need the dependency (v) to be in the list before it's being depended on
// (by u). We could reverse the result of topSortDFS; or, and this is what was
// done here, append instead of prepend `n` at the end of visit().

type state struct {
	marked map[*desc.FileDescriptor]bool // false: temporary, true: permanent
	list   []*desc.FileDescriptor
}

func topSortDFS(fds []*desc.FileDescriptor) []*desc.FileDescriptor {
	s := &state{marked: map[*desc.FileDescriptor]bool{}}
	var n *desc.FileDescriptor

	for len(fds) > 0 { // use input arg for "unmarked"
		n, fds = fds[0], fds[1:] // pick first
		s.visit(n)
	}

	return s.list
}

func (s *state) visit(n *desc.FileDescriptor) {
	perm, ok := s.marked[n]
	if ok && perm {
		return
	}
	if ok && !perm {
		panic("cycle detected")
	}
	// no mark yet
	s.mark(n, false)
	for _, m := range n.GetDependencies() {
		s.visit(m)
	}
	s.mark(n, true)
	s.list = append(s.list, n) // see comment above
}

func (s *state) mark(fd *desc.FileDescriptor, permanent bool) {
	s.marked[fd] = permanent
}
