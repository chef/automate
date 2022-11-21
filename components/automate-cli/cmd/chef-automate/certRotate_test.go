package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

const (
	RemoteFilePath = "198.51.100.0:/home/ec2-user/certs/public.pem"
	LocalFilePath  = "/home/ec2-user/certs/public.pem"
	ValidIP        = "198.51.100.0"
)

func TestIsRemotePath(t *testing.T) {
	t.Run("Valid remote path", func(t *testing.T) {
		input := RemoteFilePath
		res := IsRemotePath(input)
		assert.True(t, res)
	})

	t.Run("Local path instead of remote path", func(t *testing.T) {
		input := LocalFilePath
		res := IsRemotePath(input)
		assert.False(t, res)
	})

	t.Run("Invalid remote path 1", func(t *testing.T) {
		input := "/home/ec2-user/certs/public.pem198.51.100.0"
		res := IsRemotePath(input)
		assert.False(t, res)
	})
	t.Run("Invalid remote path 2", func(t *testing.T) {
		input := "198.51.100.0/home/ec2-user/certs/public.pem"
		res := IsRemotePath(input)
		assert.False(t, res)
	})
	t.Run("Invalid remote path 3", func(t *testing.T) {
		input := "\n   198.51.100.0:/home/ec2-user/certs/public.pem"
		res := IsRemotePath(input)
		assert.False(t, res)
	})
}

func TestGetIPV4(t *testing.T) {
	t.Run("Valid IP V4", func(t *testing.T) {
		input := RemoteFilePath
		res := GetIPV4(input)
		expected := ValidIP
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
		input := RemoteFilePath
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.NoError(t, err)
		remoteFilePathExp := LocalFilePath
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := "public.pem"
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ValidIP
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - Local path", func(t *testing.T) {
		input := LocalFilePath
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
		input := "198.51.100.0/home/ec2-user/certs/public.pem"
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.Error(t, err)
		assert.Equal(t, "Invalid remote path: 198.51.100.0/home/ec2-user/certs/public.pem", err.Error())
		remoteFilePathExp := ""
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ""
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ""
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - No filename", func(t *testing.T) {
		input := "198.51.100.0:/home/ec2-user/certs/public/"
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.NoError(t, err)
		remoteFilePathExp := "/home/ec2-user/certs/public"
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := "public"
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ValidIP
		assert.Equal(t, hostIPExp, hostIPRes)
	})

	t.Run("Invalid Remote Path - Reverse", func(t *testing.T) {
		input := "/home/ec2-user/certs/public/:198.51.100.0"
		remoteFilePathRes, fileNameRes, hostIPRes, err := GetRemoteFileDetails(input)
		assert.NoError(t, err)
		remoteFilePathExp := ValidIP
		assert.Equal(t, remoteFilePathExp, remoteFilePathRes)
		fileNameExp := ValidIP
		assert.Equal(t, fileNameExp, fileNameRes)
		hostIPExp := ValidIP
		assert.Equal(t, hostIPExp, hostIPRes)
	})

}
