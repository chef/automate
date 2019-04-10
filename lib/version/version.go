// Copyright:: Copyright 2019, Chef Software Inc.
//
// package version holds version information for Automate
// services. The version data is passed via build-time linker flags.
package version

var (
	// Version is analogous to our Habitat pkg_release.  We use
	// build-timestamps as our version.
	Version = "unknown"
	// GitSHA is the git ref that the application as built from.
	GitSHA = "unknown"

	// This is here in case we want to start using more
	// human-readable Version strings in the future.
	BuildTime = "unknown"
)
