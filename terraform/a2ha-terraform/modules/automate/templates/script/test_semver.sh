source ./semver.sh
# Function to assert equality of output
assert_equals() {
    local expected="$1"
    local actual="$2"
    if [ "$expected" == "$actual" ]; then
        echo "Test Passed"
    else
        echo "Test Failed: Expected '$expected', but got '$actual'"
        exit 1
    fi
}

# Test case 1: Installed version is greater than airgap bundle version
test_case_1() {
    echo "Running Test Case 1: Installed version > Airgap bundle version"
    result=$(semver_version_check "4.13.0" "4.12.144")
    assert_equals "4.13.0 is greater than 4.12.144" "$result"
}

# Test case 2: Airgap bundle version is greater than installed version
test_case_2() {
    echo "Running Test Case 2: Airgap bundle version > Installed version"
    result=$(semver_version_check "4.12.144" "4.13.0")
    assert_equals "4.13.0 is greater than 4.12.144, proceeding for upgrade" "$result"
}

# Test case 3: Both versions are equal
test_case_3() {
    echo "Running Test Case 3: Both versions are equal"
    result=$(semver_version_check "4.12.144" "4.12.144")
    assert_equals "Both versions are equal" "$result"
}

# Test case 4: Different patch version
test_case_4() {
    echo "Running Test Case 4: Installed version has greater patch version"
    result=$(semver_version_check "4.12.145" "4.12.144")
    assert_equals "4.12.145 is greater than 4.12.144" "$result"
}

# Test case 5: Different minor version
test_case_5() {
    echo "Running Test Case 5: Airgap bundle has greater minor version"
    result=$(semver_version_check "4.11.144" "4.12.0")
    assert_equals "4.12.0 is greater than 4.11.144, proceeding for upgrade" "$result"
}

# Test case 6: Different major minor and patch version
test_case_6() {
    echo "Running Test Case 6: Airgap bundle has greater major version"
    result=$(semver_version_check "4.11.144" "5.12.99")
    assert_equals "5.12.99 is greater than 4.11.144, proceeding for upgrade" "$result"
}
# Run all test cases
test_case_1
test_case_2
test_case_3
test_case_4
test_case_5
test_case_6
echo "All tests passed!"