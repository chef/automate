package fileutils_test

import (
	"io/fs"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/fileutils"
)

func TestPathExist(t *testing.T) {
	t.Run("it returns true if the path exists", func(t *testing.T) {
		tmpdir := t.TempDir()

		res, err := fileutils.PathExists(tmpdir)
		require.NoError(t, err)
		assert.True(t, res)
	})

	t.Run("it returns false if the path doesn't exist", func(t *testing.T) {
		res, err := fileutils.PathExists("/path/that/really/should/never/exist")
		require.NoError(t, err)
		assert.False(t, res)
	})
}

func TestIsSymlink(t *testing.T) {
	tmpdir := t.TempDir()

	testData := []byte("test data")
	filePath := path.Join(tmpdir, "file")
	ioutil.WriteFile(filePath, testData, 0700)
	symlinkPath := path.Join(tmpdir, "symlink")
	err := os.Symlink(filePath, symlinkPath)
	require.NoError(t, err)

	t.Run("it returns true for a symlink", func(t *testing.T) {
		isSymlink, err := fileutils.IsSymlink(symlinkPath)
		require.NoError(t, err)
		assert.True(t, isSymlink, "%s should be a symlink", symlinkPath)
	})

	t.Run("it returns false for an ordinary file", func(t *testing.T) {
		isSymlink, err := fileutils.IsSymlink(filePath)
		require.NoError(t, err)
		assert.False(t, isSymlink, "%s should not be a symlink", filePath)
	})
}

func TestCreateTempFile(t *testing.T) {
	content := "abc"
	filename := "file-name"
	t.Run("It creates the file", func(t *testing.T) {
		res1, err := fileutils.CreateTempFile(content, filename)
		require.NoError(t, err)
		res2, err := fileutils.CreateTempFile(content, filename)
		require.NoError(t, err)
		res3, err := fileutils.CreateTempFile(content, filename)
		require.NoError(t, err)
		res4, err := fileutils.CreateTempFile(content, filename)
		require.NoError(t, err)
		assert.Contains(t, res1, "file-name")
		assert.Contains(t, res2, "file-name")
		assert.Contains(t, res3, "file-name")
		assert.Contains(t, res4, "file-name")
	})
}

func TestDeleteTempFile(t *testing.T) {
	tempFile, err := os.CreateTemp("", "testfile")
	if err != nil {
		return
	}
	defer tempFile.Close()

	err = fileutils.DeleteFile(tempFile.Name())
	if err != nil {
		return
	}
}

func TestMove(t *testing.T) {
	content := "abc"
	filename := "file-name"
	destinationDir := createTempDir(t)
	defer os.RemoveAll(destinationDir)

	t.Run("Move a file to new directory", func(t *testing.T) {
		srcFile, err := fileutils.CreateTempFile(content, filename)
		assert.Contains(t, srcFile, "file-name")
		require.NoError(t, err)
		defer fileutils.DeleteFile(srcFile)

		err = fileutils.Move(srcFile, destinationDir+filename)
		defer fileutils.DeleteFile(destinationDir + filename)
		require.NoError(t, err)
		fileExists, err := fileutils.PathExists(destinationDir + filename)
		require.NoError(t, err)
		assert.True(t, fileExists)
	})

	t.Run("Move a file to a directory and overwrite the existing file", func(t *testing.T) {
		srcFile, err := fileutils.CreateTempFile(content, filename)
		assert.Contains(t, srcFile, "file-name")
		require.NoError(t, err)
		defer fileutils.DeleteFile(srcFile)

		existingFile := filepath.Join(destinationDir, "existing.txt")
		err = fileutils.WriteFile(existingFile, []byte("Existing File"), 0644)
		require.NoError(t, err)
		err = fileutils.Move(srcFile, existingFile)
		require.NoError(t, err)
	})

	t.Run("Invalid destinationfile", func(t *testing.T) {
		srcFile, err := fileutils.CreateTempFile(content, filename)
		assert.Contains(t, srcFile, "file-name")
		require.NoError(t, err)
		defer fileutils.DeleteFile(srcFile)

		invalidSourceFile := "/path/to/invalid/source.txt"
		err = fileutils.Move(invalidSourceFile, destinationDir+filename)
		linkErr, _ := err.(*os.LinkError)
		value := linkErr.Err.Error()
		require.Contains(t, value, "no such file or directory", "No such file or Directory")
	})
}

func createTempDir(t *testing.T) string {
	dir, err := ioutil.TempDir("", "destination_*")
	if err != nil {
		t.Fatalf("Failed to create temporary directory: %v", err)
	}
	return dir
}

func TestRemoveFirstLine(t *testing.T) {
	content := "HeaderToBeRemoved\nabc"
	filename := "file-name"

	t.Run("Remove first line", func(t *testing.T) {
		res, err := fileutils.CreateTempFile(content, filename)
		assert.Contains(t, res, "file-name")
		require.NoError(t, err)
		err = fileutils.RemoveFirstLine(res)
		require.NoError(t, err)
		fileContent, err := fileutils.ReadFile(res)
		require.NoError(t, err)
		assert.EqualValues(t, string(fileContent), "abc\n")
	})

	t.Run("Open file error", func(t *testing.T) {
		err := fileutils.RemoveFirstLine(filename)
		require.Error(t, err.(*fs.PathError), "No such file or directory")
	})
}

func TestCreateTomlFileFromConfig(t *testing.T) {
	tomlFilePath := "file-name"

	user := struct {
		Name string
		Age  int
	}{
		Name: "John Doe",
		Age:  30,
	}

	expectedContent := `Name = "John Doe"
Age = 30
`
	defer os.Remove(tomlFilePath)

	t.Run("Create toml file", func(t *testing.T) {
		tomlFile, err := fileutils.CreateTomlFileFromConfig(user, tomlFilePath)
		require.NoError(t, err)
		fileByte, err := fileutils.ReadFile(tomlFile)
		require.NoError(t, err)
		assert.Contains(t, string(fileByte), expectedContent)
		// err = fileutils.DeleteTempFile(tomlFile)
		require.NoError(t, err)
	})

	invalidFile := "/path/to/invalid/file.toml"

	t.Run("Open file error", func(t *testing.T) {
		_, err := fileutils.CreateTomlFileFromConfig(user, invalidFile)
		// fmt.Print(err.(*fs.PathError))
		require.Error(t, err, "Failed to create/open the file")
	})

	t.Run("Error encoding the config", func(t *testing.T) {
		var invalidConfig int
		_, err := fileutils.CreateTomlFileFromConfig(invalidConfig, tomlFilePath)
		require.Error(t, err, "Failed to encode")
	})
}
