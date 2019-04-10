package oidc_test

import (
	"testing"
	"time"

	go_oidc "github.com/coreos/go-oidc"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/session-service/oidc"
)

func TestTokenFromIDToken(t *testing.T) {
	rawIDToken := `eyJhbGciOiJSUzI1NiIsImtpZCI6ImJkOWY5ZGFkZDc4ZDEyOWFlN2I2ODZhZTU0NjJhOWYzY2JmMDY1MTUifQ.eyJpc3MiOiJodHRwOi8vbG9jYWxob3N0OjQyMDAvZGV4Iiwic3ViIjoiQ2cwd0xUTTROUzB5T0RBNE9TMHdFZ1J0YjJOciIsImF1ZCI6ImF1dG9tYXRlLXVpIiwiZXhwIjoxNTA5NzIwMTgzLCJpYXQiOjE1MDk2MzM3ODMsImF0X2hhc2giOiJ4ck1fTXNmLUd1dmY1dzRGeWY1THVRIiwiZW1haWwiOiJraWxnb3JlQGtpbGdvcmUudHJvdXQiLCJlbWFpbF92ZXJpZmllZCI6dHJ1ZSwiZ3JvdXBzIjpbImF1dGhvcnMiXSwibmFtZSI6IktpbGdvcmUgVHJvdXQifQ.CsBjk47MdwpkneBsbc9NEIx8TskokPDrd3Bp-C4GhcdC-eZH-vOKBnRytMi7_GcOchevo7KCmwjzZllC-AgJMd7b5SBWVjDzLQuS8D9zIX_t_vf3c_wwl4R_fYjBiO7wmm3u-VQGCmxX4UjqyfzWCT-FYwLH5WctVusM3bdlAF0FiLndkmiyAaNFbxMznlDwmrys39in4oV9srxZnXrK-ydlhpJJzETrwBVmAhDzKJO62GC6WcFQYFeQ0Dtb6eBSFaRBi7LmM5TUT_qcIW-LRGcfa7h2DfifKEgCFuv6QjUXb8B7fxRZNMQyAcoVV9qZK8Nd51l-anDD1PI4J12hyw` // nolint: lll
	exp := time.Now().Add(time.Minute)
	idToken := &go_oidc.IDToken{Expiry: exp}
	refreshToken := "randomgibberishstring"

	actual, err := oidc.TokenFromIDToken(idToken, rawIDToken, refreshToken)

	if assert.Nil(t, err) {
		assert.Equal(t, refreshToken, actual.RefreshToken)
		assert.Equal(t, exp, actual.Expiry)

		idTokenActual, ok := actual.Extra("id_token").(string)
		if assert.True(t, ok) {
			assert.Equal(t, rawIDToken, idTokenActual)
		}
	}
}
