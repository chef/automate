package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

const TEST_IP_2 = "192.0.2.12"
const TEST_IP_3 = "192.0.2.13"
const TEST_IP_4 = "192.0.2.14"
const TEST_IP_5 = "192.0.2.15"
const TEST_IP_6 = "192.0.2.16"

func TestTrimSliceSpace(t *testing.T) {
	testArr := []string{TEST_IP_1 + " ", " " + TEST_IP_3 + " "}
	resultArr := trimSliceSpace(testArr)
	assert.Equal(t, TEST_IP_1, resultArr[0])
	assert.Equal(t, TEST_IP_3, resultArr[1])
}

func TestModifyConfigForAddNewNode(t *testing.T) {
	incount := "2"
	existingIps := []string{TEST_IP_2, TEST_IP_3}
	newIps := []string{TEST_IP_4}
	certs := []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	err := modifyConfigForAddNewNode(&incount, &existingIps, newIps, &certs)
	assert.NoError(t, err)
	assert.Equal(t, "3", incount)
	assert.Equal(t, []string{TEST_IP_2, TEST_IP_3, TEST_IP_4}, existingIps)
	assert.Equal(t, CertByIP{
		IP:         TEST_IP_4,
		PrivateKey: "private",
		PublicKey:  "public",
	}, certs[2])
}

func TestModifyConfigForDeleteNode(t *testing.T) {
	incount := "2"
	existingIps := []string{TEST_IP_2, TEST_IP_3}
	newIps := []string{TEST_IP_3}
	certs := []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	err := modifyConfigForDeleteNode(&incount, &existingIps, newIps, &certs)
	assert.NoError(t, err)
	assert.Equal(t, "1", incount)
	assert.Equal(t, []string{TEST_IP_2}, existingIps)
	assert.Equal(t, 1, len(certs))
}

func TestDifference(t *testing.T) {
	a := []string{TEST_IP_2, TEST_IP_3, TEST_IP_5}
	b := []string{TEST_IP_3}
	strArr := difference(a, b)
	assert.Equal(t, []string{TEST_IP_2, TEST_IP_5}, strArr)
}

func TestDifference1(t *testing.T) {
	a := []string{TEST_IP_2, TEST_IP_3, TEST_IP_5}
	b := []string{TEST_IP_3, TEST_IP_5}
	strArr := difference(a, b)
	assert.Equal(t, []string{TEST_IP_2}, strArr)
}

func TestDifferenceIfNoMatch(t *testing.T) {
	a := []string{TEST_IP_2, TEST_IP_3, TEST_IP_5}
	b := []string{TEST_IP_1, TEST_IP_6}
	strArr := difference(a, b)
	assert.Equal(t, []string{TEST_IP_2, TEST_IP_3, TEST_IP_5}, strArr)
}

func TestFindAndDelete(t *testing.T) {
	certs := []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		},
		{
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	b := TEST_IP_3
	arr := findAndDelete(certs, b)
	assert.Equal(t, []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}, arr)
}

func TestFindAndDeleteIfNoMatch(t *testing.T) {
	certs := []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		}, {
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		}, {
			IP:         TEST_IP_6,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}
	b := TEST_IP_1
	arr := findAndDelete(certs, b)
	assert.Equal(t, []CertByIP{
		{
			IP:         TEST_IP_2,
			PrivateKey: "private",
			PublicKey:  "public",
		}, {
			IP:         TEST_IP_3,
			PrivateKey: "private",
			PublicKey:  "public",
		}, {
			IP:         TEST_IP_6,
			PrivateKey: "private",
			PublicKey:  "public",
		},
	}, arr)
}

func TestModifyInstanceCount(t *testing.T) {
	count, err := modifyInstanceCount("3", 1)
	assert.NoError(t, err)
	assert.Equal(t, "4", count)

	count, err = modifyInstanceCount("3", -2)
	assert.NoError(t, err)
	assert.Equal(t, "1", count)
}

func TestSplitIPCSV(t *testing.T) {
	automateIpList, chefServerIpList, opensearchIpList, postgresqlIp := splitIPCSV(TEST_IP_1+","+TEST_IP_2, TEST_IP_1+","+TEST_IP_2, TEST_IP_1+","+TEST_IP_2, TEST_IP_1+","+TEST_IP_2)
	assert.Equal(t, []string{TEST_IP_1, TEST_IP_2}, automateIpList)
	assert.Equal(t, []string{TEST_IP_1, TEST_IP_2}, chefServerIpList)
	assert.Equal(t, []string{TEST_IP_1, TEST_IP_2}, opensearchIpList)
	assert.Equal(t, []string{TEST_IP_1, TEST_IP_2}, postgresqlIp)
}

func TestIsFinalInstanceCountAllowed(t *testing.T) {
	allowed, finalcount, err := isFinalInstanceCountAllowed("3", -1, 3)
	assert.NoError(t, err)
	assert.False(t, allowed)
	assert.Equal(t, 2, finalcount)

	allowed, finalcount, err = isFinalInstanceCountAllowed("5", -2, 3)
	assert.NoError(t, err)
	assert.True(t, allowed)
	assert.Equal(t, 3, finalcount)
}
