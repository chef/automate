package gateway

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIsFileNameValid(t *testing.T) {
	tests := []struct {
		fileName string
		expected bool
	}{
		{"myfile.txt", true},
		{"file<name>.txt", false},
		{"file:name.txt", false},
		{"file/name.txt", false},
		{"file|name.txt", false},
		{"file?name.txt", false},
		{"file*name.txt", false},
		{"file\x00name.txt", false},
		{"file\nname.txt", false},
		{"file!name.txt", false},
		{"file&name.txt", false},
		{"file%name.txt", false},
		{"file#@name.txt", false},
		{"file[invalid]name.txt", true},
		{"file{invalid}name.txt", true},
		{"file(invalid)name.txt", true},
		{"file.invalid.name.txt", true},
		{"file_underscore_name.txt", true},
		{"file-hyphen-name.txt", true},
		{"file123name.txt", true},
		{"file   spaces   name.txt", true},
	}

	for _, test := range tests {
		t.Run(fmt.Sprintf("File name: %s", test.fileName), func(t *testing.T) {
			result := IsFileNameValid(test.fileName)
			assert.Equal(t, test.expected, result)
		})
	}
}

func TestRemoveFilePath(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"path/to/file.txt", "file.txt"},
		{"path/to/my file.txt", "my file.txt"},
		{"path/to/my_file.txt", "my_file.txt"},
		{"path/to/file with space.txt", "file with space.txt"},
		{"path/to/invalid_file.txt", "invalid_file.txt"},
		{"error during zip to tar.gz conversion: open /hab/svc/compliance-service/var/tmp/inspec-upload2104240454/__MACOSX/._ncctest && curl 22.188.48.229: no such file or directory", "error during zip to tar.gz conversion: open /._ncctest && curl 22.188.48.229: no such file or directory"},
	}

	for _, test := range tests {
		t.Run(fmt.Sprintf("Input: %s", test.input), func(t *testing.T) {
			result := RemoveFilePath(test.input)
			assert.Equal(t, test.expected, result)
		})
	}
}
