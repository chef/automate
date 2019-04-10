package util

import (
	"crypto/rand"
	"encoding/base64"
	"fmt"
)

// use as a basis for tokens. Tokens will be base64-encoded.
const PRNCount = 20

// Minimum token length for Legacy tokens imported from Automate
// 1. Technically, there may not be a minimum, but anything less than
// 2 does not seems reasonable.
const MinimumLegacyTokenLength = 2

func MinimumTokenLength() int {
	return PRNCount
}

func IsValidLegacyToken(token string) error {
	if len(token) < MinimumLegacyTokenLength {
		return fmt.Errorf(
			"legacy token %q is invalid; it is length %d but must be at least %d characters",
			token, len(token), MinimumLegacyTokenLength)
	}

	return nil
}

func IsValidToken(token string) error {
	if len(token) < MinimumTokenLength() {
		return fmt.Errorf(
			"token %q is invalid; it is length %d but must be at least %d characters",
			token, len(token), PRNCount)
	}

	return nil
}

func GenerateNewToken() (string, error) {
	b := make([]byte, PRNCount)
	_, err := rand.Read(b)
	if err != nil {
		return "", err
	}
	return base64.URLEncoding.EncodeToString(b), nil
}
