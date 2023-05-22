package getosutils

import (
	"strconv"
	"strings"

	"github.com/chef/automate/lib/io/fileutils"
)

func GetOsVersion(osFilepath string) (string, string, error) {
	data, err := fileutils.ReadFile(osFilepath)
	if err != nil {
		return "", "", err
	}
	var name, version string
	lines := strings.Split(string(data), "\n")

	for _, line := range lines {
		if strings.HasPrefix(line, "PRETTY_NAME=") {
			name = strings.TrimPrefix(line, "PRETTY_NAME=")
			name = strings.Trim(name, `"`)
		} else if strings.HasPrefix(line, "VERSION_ID=") {
			version = strings.TrimPrefix(line, "VERSION_ID=")
			version = strings.Trim(version, `"`)
		}
	}
	return name, version, nil
}

func GetKernelVersion(kernelFilepath string) (string, error) {
	data, err := fileutils.ReadFile(kernelFilepath)
	if err != nil {
		return "", err
	}
	kernelVersion := string(data)
	split := strings.Split(kernelVersion, ".")
	kernelVersion = split[0] + "." + split[1]
	return kernelVersion, nil
}

func GetCPUSpeed(cpuInfoFile string) (float64, error) {
	cpuInfo, err := fileutils.ReadFile(cpuInfoFile)
	if err != nil {
		return float64(0), err
	}

	lines := strings.Split(string(cpuInfo), "\n")
	var cpuSpeed float64

	for _, line := range lines {
		if strings.HasPrefix(line, "cpu MHz") {
			parts := strings.Split(line, ":")
			cpuSpeed, err = strconv.ParseFloat(strings.TrimSpace(parts[1]), 64)
			if err != nil {
				return float64(0), err
			}
			break
		}
	}
	return cpuSpeed / 1000, nil
}
