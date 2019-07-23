package oidc

import (
	"errors"
	"net/http"
	"strings"
)

// ExtractBearerToken is a RequestTokenExtractor which extracts a bearer token from a request's
// Authorization header.
//
// Note: copied from github.com/cores/go-oidc@v1,
// https://github.com/coreos/go-oidc/blob/e860bd55bfa7d/oidc/util.go#L20-L38
func ExtractBearerToken(r *http.Request) (string, error) {
	ah := r.Header.Get("Authorization")
	if ah == "" {
		return "", errors.New("missing Authorization header")
	}

	if len(ah) <= 6 || strings.ToUpper(ah[0:6]) != "BEARER" {
		return "", errors.New("should be a bearer token")
	}

	val := ah[7:]
	if len(val) == 0 {
		return "", errors.New("bearer token is empty")
	}

	return val, nil
}
