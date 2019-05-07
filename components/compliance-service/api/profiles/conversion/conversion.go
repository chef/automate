package conversion

import (
	"encoding/json"

	"sort"

	"github.com/chef/automate/components/compliance-service/api/profiles"
	"github.com/chef/automate/components/compliance-service/inspec"
)

func ConvertToPSProfile(profile inspec.Profile, namespace string) (profiles.Profile, error) {
	var convertedProfile profiles.Profile
	convertedProfile.Name = profile.Name
	convertedProfile.Title = profile.Title
	convertedProfile.Version = profile.Version
	convertedProfile.Summary = profile.Summary
	convertedProfile.Maintainer = profile.Maintainer
	convertedProfile.License = profile.License
	convertedProfile.Copyright = profile.Copyright
	convertedProfile.CopyrightEmail = profile.CopyrightEmail
	convertedProfile.Sha256 = profile.Sha256
	convertedProfile.Owner = namespace

	var convertedSupports []*profiles.Support
	for _, support := range profile.Supports {
		family := support["os-family"]
		if len(family) == 0 {
			family = support["platform-family"]
		}
		name := support["os-name"]
		if len(name) == 0 {
			name = support["platform-name"]
		}
		convertedSupports = append(convertedSupports, &profiles.Support{
			OsFamily:      family,
			OsName:        name,
			Platform:      support["platform"],
			Release:       support["release"],
			InspecVersion: support["inspec-version"],
		})
	}
	convertedProfile.Supports = convertedSupports

	var convertedDepends []*profiles.Dependency
	jsonDepends, err := json.Marshal(profile.Dependencies)
	if err != nil {
		return convertedProfile, err
	}
	err = json.Unmarshal(jsonDepends, &convertedDepends)
	if err == nil {
		convertedProfile.Depends = convertedDepends
	}

	var convertedGroups []*profiles.Group
	jsonGroups, err := json.Marshal(profile.Groups)
	if err != nil {
		return convertedProfile, err
	}
	err = json.Unmarshal(jsonGroups, &convertedGroups)
	if err == nil {
		convertedProfile.Groups = convertedGroups
	}

	var convertedAttributes []*profiles.Attribute
	jsonAttributes, err := json.Marshal(profile.Attributes)
	if err != nil {
		return convertedProfile, err
	}
	err = json.Unmarshal(jsonAttributes, &convertedAttributes)
	if err == nil {
		convertedProfile.Attributes = convertedAttributes
	}

	var convertedControls []*profiles.Control
	for _, control := range profile.Controls {
		convertedControl := profiles.Control{
			Id:     control.ID,
			Code:   control.Code,
			Desc:   control.Desc,
			Impact: control.Impact,
			Title:  control.Title,
		}

		var convertedSourceLocation *profiles.SourceLocation
		jsonSourceLocation, err := json.Marshal(control.SourceLocation)
		if err != nil {
			return convertedProfile, err
		}
		err = json.Unmarshal(jsonSourceLocation, convertedSourceLocation)
		if err == nil {
			convertedControl.SourceLocation = convertedSourceLocation
		}

		var convertedTags map[string]string
		jsonTags, err := json.Marshal(control.Tags)
		if err != nil {
			return convertedProfile, err
		}
		err = json.Unmarshal(jsonTags, &convertedTags)
		if err == nil {
			convertedControl.Tags = convertedTags
		}

		var convertedRefs []*profiles.Ref
		jsonRefs, err := json.Marshal(control.Refs)
		if err != nil {
			return convertedProfile, err
		}
		err = json.Unmarshal(jsonRefs, &convertedRefs)
		if err != nil {
			convertedControl.Refs = convertedRefs
		}

		var convertedResults []*profiles.Result
		for _, result := range control.Results {
			convertedResult := profiles.Result{
				Status:      result.Status,
				CodeDesc:    result.CodeDesc,
				RunTime:     result.RunTime,
				StartTime:   result.StartTime,
				Message:     result.Message,
				SkipMessage: result.SkipMessage,
			}
			convertedResults = append(convertedResults, &convertedResult)
		}
		convertedControl.Results = convertedResults
		convertedControls = append(convertedControls, &convertedControl)
	}
	// Sort profile controls by Id
	sort.Slice(convertedControls, func(i, j int) bool {
		return convertedControls[i].Id < convertedControls[j].Id
	})
	convertedProfile.Controls = convertedControls
	return convertedProfile, nil
}

func ConvertInspecMetadatasToProfiles(inspecMetadata []inspec.Metadata, owner string) *profiles.Profiles {
	profs := &profiles.Profiles{}
	profs.Profiles = []*profiles.Profile{}
	for _, metadata := range inspecMetadata {
		profs.Profiles = append(profs.Profiles, InspectMetadataToProfile(metadata, owner))
	}
	profs.Total = int32(len(profs.Profiles))
	return profs
}

func InspectMetadataToProfile(metadata inspec.Metadata, owner string) *profiles.Profile {
	return &profiles.Profile{
		Name:           metadata.Name,
		Title:          metadata.Title,
		Maintainer:     metadata.Maintainer,
		Copyright:      metadata.Copyright,
		CopyrightEmail: metadata.Copyright_Email,
		License:        metadata.License,
		Summary:        metadata.Summary,
		Version:        metadata.Version,
		Supports:       ConvertInspecSupportsToProfileSupports(metadata.Supports),
		Depends:        ConvertInspecDependenciesToProfileDependencies(metadata.Dependencies),
		LatestVersion:  metadata.LatestVersion,
		Owner:          owner,
		Sha256:         metadata.Sha256,
	}
}

func ConvertInspecSupportsToProfileSupports(inspecSupports []map[string]string) (supports []*profiles.Support) {
	for _, inspecSupport := range inspecSupports {
		supports = append(supports, InspecSupportToProfileSupport(inspecSupport))
	}
	return
}

func InspecSupportToProfileSupport(inspecSupport map[string]string) (support *profiles.Support) {
	return &profiles.Support{
		OsName:        inspecSupport["os-name"],
		OsFamily:      inspecSupport["os-family"],
		Platform:      inspecSupport["platform"],
		Release:       inspecSupport["release"],
		InspecVersion: inspecSupport["inspec-version"],
	}
}

func ConvertInspecDependenciesToProfileDependencies(inspecDependencies []inspec.Dependency) (supports []*profiles.Dependency) {
	for _, inspecDependency := range inspecDependencies {
		supports = append(supports, InspecDependencyToProfileDependency(&inspecDependency))
	}
	return
}

func InspecDependencyToProfileDependency(inspecDependency *inspec.Dependency) (dependency *profiles.Dependency) {
	return &profiles.Dependency{
		Name:        inspecDependency.Name,
		Url:         inspecDependency.URL,
		Path:        inspecDependency.Path,
		Git:         inspecDependency.Git,
		Branch:      inspecDependency.Branch,
		Tag:         inspecDependency.Tag,
		Commit:      inspecDependency.Commit,
		Version:     inspecDependency.Version,
		Supermarket: inspecDependency.Supermarket,
		Github:      inspecDependency.Github,
		Compliance:  inspecDependency.Compliance,
	}
}
