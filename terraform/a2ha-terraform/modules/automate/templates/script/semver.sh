semver_version_check() {
    installed_version=$1
    airgap_bundle_version=$2

    IFS='.' read -r major1 minor1 patch1 <<< "$installed_version"
    IFS='.' read -r major2 minor2 patch2 <<< "$airgap_bundle_version"

    # Compare major versions
    if (( major1 > major2 )); then
        echo "$installed_version is greater than $airgap_bundle_version"
        isSkipRequired=true
    elif (( major1 < major2 )); then
        echo "$airgap_bundle_version is greater than $installed_version, proceeding for upgrade"
    else
        # Compare minor versions
        if (( minor1 > minor2 )); then
            echo "$installed_version is greater than $airgap_bundle_version"
            isSkipRequired=true
        elif (( minor1 < minor2 )); then
            echo "$airgap_bundle_version is greater than $installed_version, proceeding for upgrade"
        else
            # Compare patch versions
            if (( patch1 > patch2 )); then
                echo "$installed_version is greater than $airgap_bundle_version"
                isSkipRequired=true
            elif (( patch1 < patch2 )); then
                echo "$airgap_bundle_version is greater than $installed_version, proceeding for upgrade"
            else
                echo "Both versions are equal"
                isSkipRequired=true
            fi
        fi
    fi
}