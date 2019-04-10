// +build !amd64

package secureconn

// The upstream Golang implementation is only defined on amd64 and
// s390x and we've only copied over the amd64 version since we don't
// build on s390x currently.

// HasAESNI returns whether AES-NI is supported by the CPU.
func HasAESNI() bool {
	return false
}
