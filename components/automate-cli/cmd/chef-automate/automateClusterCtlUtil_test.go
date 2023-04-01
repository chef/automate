package main

import (
	"bytes"
	"io/ioutil"
	"os"
	"testing"

	"github.com/chef/automate/components/automate-deployment/pkg/airgap"
	"github.com/stretchr/testify/require"
)

func TestCopyFileContents(t *testing.T) {
	srcFile, err := ioutil.TempFile("", "source_file_")
	require.NoError(t, err)

	defer os.Remove(srcFile.Name())

	dstFile, err := ioutil.TempFile("", "destination_file_")
	require.NoError(t, err)

	defer os.Remove(dstFile.Name())

	srcData := []byte("hello world")
	n, err := srcFile.Write(srcData)
	require.NoError(t, err)
	require.NotZero(t, n)

	err = copyFileContents(srcFile.Name(), dstFile.Name())
	require.NoError(t, err)

	dstData, err := ioutil.ReadFile(dstFile.Name())
	require.NoError(t, err)

	equal := bytes.Equal(dstData, srcData)
	require.True(t, equal)
}

func TestGenerateA2HAManifestTfvars(t *testing.T) {
	airgapMetadata := airgap.UnpackMetadata{
		HartifactPaths: []string{
			"/path/to/pgleaderchk_pkg-1.0.0-20211231235959-x86_64-linux.hart",
			"/path/to/postgresql_pkg-2.0.0-20220131235959-x86_64-linux.hart",
			"/path/to/proxy_pkg-3.0.0-20220228235959-x86_64-linux.hart",
			"/path/to/opensearch_pkg-4.0.0-20220331235959-x86_64-linux.hart",
			"/path/to/elasticsidecar_pkg-5.0.0-20220430235959-x86_64-linux.hart",
		},
	}

	err := generateA2HAManifestTfvars(airgapMetadata)
	require.NoError(t, err)

	manifestContents, err := ioutil.ReadFile(AUTOMATE_HA_TERRAFORM_DIR + "a2ha_manifest.auto.tfvars")
	require.NoError(t, err)

	expectedContents := "pgleaderchk_pkg_ident = \"core/pgleaderchk_pkg/1.0.0\"\n" +
		"postgresql_pkg_ident = \"core/postgresql_pkg/2.0.0\"\n" +
		"proxy_pkg_ident = \"core/proxy_pkg/3.0.0\"\n" +
		"opensearch_pkg_ident = \"core/opensearch_pkg/4.0.0\"\n" +
		"elasticsidecar_pkg_ident = \"core/elasticsidecar_pkg/5.0.0\"\n"
	equal := string(manifestContents) == expectedContents
	require.True(t, equal)

	err = os.Remove(AUTOMATE_HA_TERRAFORM_DIR + "a2ha_manifest_auto.tfvars")
	require.NoError(t, err)
}

func Test_checkIfFileExist(t *testing.T) {
	type args struct {
		path string
	}
	tests := []struct {
		name string
		args args
		want bool
	}{
		// TODO: Add test cases.
		{
			name: "test one",
			args: args{path: "/abc/qwe.txt"},
			want: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := checkIfFileExist(tt.args.path); got != tt.want {
				t.Errorf("checkIfFileExist() = %v, want %v", got, tt.want)
			}
		})
	}
}
