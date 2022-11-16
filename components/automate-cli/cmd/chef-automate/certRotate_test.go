package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIsRemotePath(t *testing.T) {
	t.Run("Valid remote path", func(t *testing.T) {
		input := "10.1.0.234:/home/ec2-user/certs/public.pem"
		res := IsRemotePath(input)
		assert.True(t, res)
	})

	t.Run("Local path instead of remote path", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem"
		res := IsRemotePath(input)
		assert.False(t, res)
	})

	t.Run("Invalid remote path 1", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem10.1.0.234"
		res := IsRemotePath(input)
		assert.False(t, res)
	})
	t.Run("Invalid remote path 2", func(t *testing.T) {
		input := "10.1.0.234/home/ec2-user/certs/public.pem"
		res := IsRemotePath(input)
		assert.False(t, res)
	})
	t.Run("Invalid remote path 3", func(t *testing.T) {
		input := "\n   10.1.0.234:/home/ec2-user/certs/public.pem"
		res := IsRemotePath(input)
		assert.False(t, res)
	})
}

func TestIsValidFilePath(t *testing.T) {
	t.Run("Valid file path", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem"
		res := IsValidFilePath(input)
		assert.True(t, res)
	})

	t.Run("Valid file path", func(t *testing.T) {
		input := "/public.pem"
		res := IsValidFilePath(input)
		assert.True(t, res)
	})

	t.Run("Valid file path", func(t *testing.T) {
		input := "public.pem"
		res := IsValidFilePath(input)
		assert.True(t, res)
	})

	t.Run("Invalid file path - remote path", func(t *testing.T) {
		input := "10.1.0.234:/home/ec2-user/certs/public.pem"
		res := IsValidFilePath(input)
		assert.False(t, res)
	})

	t.Run("Invalid file path 1", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem:10.1.0.234"
		res := IsValidFilePath(input)
		assert.False(t, res)
	})

	t.Run("Invalid remote path 2", func(t *testing.T) {
		input := "~/public.pem"
		res := IsValidFilePath(input)
		assert.False(t, res)
	})

	t.Run("Invalid remote path 3", func(t *testing.T) {
		input := "./public.pem"
		res := IsValidFilePath(input)
		assert.False(t, res)
	})
}

func TestGetIPV4(t *testing.T) {
	t.Run("Valid IP V4", func(t *testing.T) {
		input := "10.1.0.234:/home/ec2-user/certs/public.pem"
		res := GetIPV4(input)
		expected := "10.1.0.234"
		assert.Equal(t, expected, res)
	})

	t.Run("Valid IP V4 but invalid remote path 1", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem:127.0.0.1"
		res := GetIPV4(input)
		expected := "127.0.0.1"
		assert.Equal(t, expected, res)
	})

	t.Run("Valid IP V4 but invalid remote path 2", func(t *testing.T) {
		input := "/home/ec2-user/:0.0.0.0:/certs/public.pem"
		res := GetIPV4(input)
		expected := "0.0.0.0"
		assert.Equal(t, expected, res)
	})

	t.Run("Invalid IP v4 and valid path", func(t *testing.T) {
		input := "256.256.256.256:/home/ec2-user/certs/public.pem"
		res := GetIPV4(input)
		expected := ""
		assert.Equal(t, expected, res)
	})

	t.Run("Invalid IP v4 and invalid path", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem:1.2.3"
		res := GetIPV4(input)
		expected := ""
		assert.Equal(t, expected, res)
	})
}

func TestGetRemoteFileDetails(t *testing.T) {
	t.Run("Valid Remote Path", func(t *testing.T) {
		input := "10.1.0.234:/home/ec2-user/certs/public.pem"
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.NoError(t, err)
		remoteFilePathExp := "/home/ec2-user/certs/public.pem"
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := "public.pem"
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := "10.1.0.234"
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - Local path", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem"
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.Error(t, err)
		assert.Equal(t, " is not a valid IPv4 address", err.Error())
		remoteFilePathExp := ""
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ""
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ""
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - Colon missing", func(t *testing.T) {
		input := "10.1.0.234/home/ec2-user/certs/public.pem"
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.Error(t, err)
		assert.Equal(t, "Invalid remote path: 10.1.0.234/home/ec2-user/certs/public.pem", err.Error())
		remoteFilePathExp := ""
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ""
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ""
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - No filename", func(t *testing.T) {
		input := "10.1.0.234:/home/ec2-user/certs/public/"
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.Error(t, err)
		assert.Equal(t, "Invalid remote file path: /home/ec2-user/certs/public/", err.Error())
		remoteFilePathExp := ""
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ""
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ""
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - No filename", func(t *testing.T) {
		input := "/home/ec2-user/certs/public/:10.1.0.234"
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.Error(t, err)
		assert.Equal(t, "Invalid remote file path: 10.1.0.234", err.Error())
		remoteFilePathExp := ""
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ""
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ""
		assert.Equal(t, hostIPExp, hostIPRes)
	})

}
