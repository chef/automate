package fileutils_test

import (
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"strings"
	"testing"
	"testing/iotest"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/io/fileutils"
)

func TestAtomicWriteSuccess(t *testing.T) {
	dir, err := ioutil.TempDir("", "AtomicWriteTest")
	require.NoError(t, err, "creating temporary dir")
	defer os.RemoveAll(dir)

	filename := path.Join(dir, "foo")
	reader := strings.NewReader("bar")
	err = fileutils.AtomicWrite(filename, reader)
	require.NoError(t, err)
	data, err := ioutil.ReadFile(filename)
	require.NoError(t, err)
	assert.Equal(t, "bar", string(data))
}

func TestAtomicWriteExecutableSuccess(t *testing.T) {
	dir, err := ioutil.TempDir("", "AtomicWriteTest")
	require.NoError(t, err, "creating temporary dir")
	defer os.RemoveAll(dir)

	filename := path.Join(dir, "foo")
	reader := strings.NewReader(`#!/bin/bash

		echo bar
	`)
	err = fileutils.AtomicWrite(filename, reader, fileutils.WithAtomicWriteFileMode(0755))
	require.NoError(t, err)
	cmd := exec.Command(filename)
	data, err := cmd.CombinedOutput()
	require.NoError(t, err)
	assert.Equal(t, "bar\n", string(data))
}

func TestAtomicWriteFail(t *testing.T) {
	dir, err := ioutil.TempDir("", "AtomicWriteTest")
	require.NoError(t, err, "creating temporary dir")
	defer os.RemoveAll(dir)

	filename := path.Join(dir, "foo")
	reader := iotest.TimeoutReader(iotest.OneByteReader(strings.NewReader("bar")))
	err = fileutils.AtomicWrite(filename, reader)
	require.Error(t, err)
	_, err = os.Stat(filename)
	assert.True(t, os.IsNotExist(err), "The file should not have been created because there was an error")
}
