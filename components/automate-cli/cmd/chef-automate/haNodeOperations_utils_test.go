package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test_trimSliceSpace(t *testing.T) {
	testArr := []string{"10.2.1.32 ", " 10.0.1.192 "}
	resultArr := trimSliceSpace(testArr)
	assert.Equal(t, "10.2.1.32", resultArr[0])
	assert.Equal(t, "10.0.1.192", resultArr[1])
}

func Test_modifyConfigForAddNewNode(t *testing.T) {
	incount := "2"
	existingIps := []string{"10.2.3.23", "10.0.1.192"}
	newIps := []string{"10.9.8.67"}
	certs := []CertByIP{
		{
			IP:         "10.2.3.23",
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         "10.0.1.192",
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	err := modifyConfigForAddNewNode(&incount, &existingIps, newIps, &certs)
	assert.NoError(t, err)
	assert.Equal(t, "3", incount)
	assert.Equal(t, []string{"10.2.3.23", "10.0.1.192", "10.9.8.67"}, existingIps)
	assert.Equal(t, CertByIP{
		IP:         "10.9.8.67",
		PrivateKey: "private",
		PublicKey:  "public",
	}, certs[2])
}

func Test_modifyConfigForDeleteNode(t *testing.T) {
	incount := "2"
	existingIps := []string{"10.2.3.23", "10.0.1.192"}
	newIps := []string{"10.0.1.192"}
	certs := []CertByIP{
		{
			IP:         "10.2.3.23",
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         "10.0.1.192",
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	err := modifyConfigForDeleteNode(&incount, &existingIps, newIps, &certs)
	assert.NoError(t, err)
	assert.Equal(t, "1", incount)
	assert.Equal(t, []string{"10.2.3.23"}, existingIps)
	assert.Equal(t, 1, len(certs))
}

func Test_difference(t *testing.T) {
	a := []string{"10.2.3.23", "10.0.1.192", "10.2.3.4"}
	b := []string{"10.0.1.192"}
	strArr := difference(a, b)
	assert.Equal(t, []string{"10.2.3.23", "10.2.3.4"}, strArr)
}

func Test_difference_1(t *testing.T) {
	a := []string{"10.2.3.23", "10.0.1.192", "10.2.3.4"}
	b := []string{"10.0.1.192", "10.2.3.4"}
	strArr := difference(a, b)
	assert.Equal(t, []string{"10.2.3.23"}, strArr)
}

func Test_findAndDelete(t *testing.T) {
	certs := []CertByIP{
		{
			IP:         "10.2.3.23",
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         "10.0.1.192",
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	b := "10.0.1.192"
	arr := findAndDelete(certs, b)
	assert.Equal(t, []CertByIP{
		{
			IP:         "10.2.3.23",
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}, arr)
}

func Test_modifyInstanceCount(t *testing.T) {
	count, err := modifyInstanceCount("3", 1)
	assert.NoError(t, err)
	assert.Equal(t, "4", count)

	count, err = modifyInstanceCount("3", -2)
	assert.NoError(t, err)
	assert.Equal(t, "1", count)
}

func Test_splitIPCSV(t *testing.T) {
	automateIpList, chefServerIpList, opensearchIpList, postgresqlIp := splitIPCSV("10.2.3.4,10.4.3.2", "10.2.3.8,10.4.3.23", "10.2.43.4,10.4.31.2", "10.2.3.99,10.4.3.200")
	assert.Equal(t, []string{"10.2.3.4", "10.4.3.2"}, automateIpList)
	assert.Equal(t, []string{"10.2.3.8", "10.4.3.23"}, chefServerIpList)
	assert.Equal(t, []string{"10.2.43.4", "10.4.31.2"}, opensearchIpList)
	assert.Equal(t, []string{"10.2.3.99", "10.4.3.200"}, postgresqlIp)
}

func Test_isFinalInstanceCountAllowed(t *testing.T) {
	allowed, finalcount, err := isFinalInstanceCountAllowed("3", -1, 3)
	assert.NoError(t, err)
	assert.False(t, allowed)
	assert.Equal(t, 2, finalcount)

	allowed, finalcount, err = isFinalInstanceCountAllowed("5", -2, 3)
	assert.NoError(t, err)
	assert.True(t, allowed)
	assert.Equal(t, 3, finalcount)
}
