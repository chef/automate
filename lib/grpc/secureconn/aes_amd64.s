// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// This code is from the Golang standard library, but the comments
// have been added since we don't deal with Go assmebly much.
//
// https://en.wikipedia.org/wiki/CPUID#EAX=1:_Processor_Info_and_Feature_Bits
// https://golang.org/doc/asm

#include "textflag.h"
// func HasAESNI() bool
TEXT ·HasAESNI(SB),NOSPLIT,$0
	XORQ AX, AX           // Clear AX
	INCL AX               // Set AX to 1, CPUID uses AX as an argument
	CPUID                 // CPUID with AX=1 returns Feature Bits in the EDX and ECX registers
	SHRQ $25, CX          // The AESNI feature flag is the 25th bit, shift it to the bottom
	ANDQ $1, CX           // If AES = 1, our CX register now has 0...01 (true), if AES=0 we have 0...00 (false)
	MOVB CX, ret+0(FP)    // Return CX since it is now a boolean that is true if AES is enabled
	RET
