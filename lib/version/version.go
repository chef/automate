// Copyright:: Copyright (c) 2019-2025 Progress Software Corporation and/or its subsidiaries or affiliates. All Rights Reserved.
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
