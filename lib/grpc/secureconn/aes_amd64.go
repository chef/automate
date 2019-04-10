// +build amd64

package secureconn

// HasAESNI returns whether AES-NI is supported by the CPU.  This
// function gets defined by the assembly in aes_amd64.s
func HasAESNI() bool
