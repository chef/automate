package nfsmountservice

import (
	"bytes"
	"errors"
	"io"
	"io/ioutil"
	"strconv"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/mockserver"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var (
	SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITH_RESULT_STRUCT = `{
		"status": "SUCCESS",
		"result": {
			"address": "10.0.0.11",
			"mount_location": "/mnt/automate_backups",
			"nfs": "10.0.0.11:/automate_backups"
		}
	}`

	SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT = models.NFSMountLocResponse{
		Address:       "10.0.0.11",
		Nfs:           "10.0.0.11:/automate_backups",
		MountLocation: "/mnt/automate_backups",
	}

	SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT2 = models.NFSMountLocResponse{
		Address:       "10.0.0.12",
		Nfs:           "10.0.0.12:/automate_backups",
		MountLocation: "/mnt/automate_backups",
	}

	NFS_NOT_MOUNTED_STRUCT = models.NFSMountLocResponse{
		Address:       "",
		Nfs:           "",
		MountLocation: "/mnt/automate_backups",
	}

	FAILED_NFS_MOUNT_LOC_RESPONSE_BODY = `{
		"status": "FAILED",
		"result": null,
		"error": {
			"code": 400,
			"message": "Bad Response"
		}
	}`

	SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITHOUT_RESULT_STRUCT = `{
		"status": "SUCCESS",
		"result": ""
	}`

	VALID_NFS_MOUNT_RESPONSE = models.NFSMountResponse{
		IP:       "whatever",
		NodeType: "automate",
		CheckList: []models.Checks{
			{
				Title:         "NFS Mount",
				Passed:        true,
				SuccessMsg:    "NFS mount location found",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		Error: nil,
	}
	VALID_NFS_MOUNT_BUT_NOT_SHARED_RESPONSE = models.NFSMountResponse{
		IP:       "whatever",
		NodeType: "automate",
		CheckList: []models.Checks{
			{
				Title:         "NFS Mount",
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "NFS mount location not found",
				ResolutionMsg: "NFS volume should be mounted on /mnt/automate_backups",
			},
		},
		Error: nil,
	}
)

func TestNFSMountService(t *testing.T) {
	nm := NewNFSMountService()
	nmDetails := nm.GetNFSMountDetails(models.NFSMountRequest{}, true)
	assert.Equal(t, new([]models.NFSMountResponse), nmDetails)
}

func TestCheckMount(t *testing.T) {
	tests := []struct {
		TestName    string
		ResultBody  *models.NFSMountLocResponse
		ExpectedRes bool
	}{
		{"NFS is Mounted", &models.NFSMountLocResponse{Address: "anything"}, true},
		{"NFS is not Mounted", &models.NFSMountLocResponse{}, false},
	}

	for _, e := range tests {
		nodeData := &models.NFSMountResponse{}
		checkMount("/mnt/automate_backups", nodeData, e.ResultBody)
		assert.Equal(t, e.ExpectedRes, nodeData.CheckList[0].Passed)
	}
}

func TestCheckShare(t *testing.T) {
	compareWith := models.NFSMountLocResponse{
		Address:       "10.0.0.11",
		Nfs:           "10.0.0.11:/backup_share",
		MountLocation: "/mnt/automate_backups",
	}
	tests := []struct {
		TestName         string
		Data             models.NFSMountLocResponse
		NfsMounted       bool
		ExpectedCheckRes bool
	}{
		{
			TestName: "NFS is mounted and Shared",
			Data: models.NFSMountLocResponse{
				Address:       "10.0.0.11",
				Nfs:           "10.0.0.11:/backup_share",
				MountLocation: "/mnt/automate_backups",
			},
			NfsMounted:       true,
			ExpectedCheckRes: true,
		},
		{
			TestName: "NFS is not mounted",
			Data: models.NFSMountLocResponse{
				Address:       "",
				Nfs:           "",
				MountLocation: "/mnt/automate_backups",
			},
			NfsMounted:       false,
			ExpectedCheckRes: false,
		},
		{
			TestName: "NFS is mounted but not shared",
			Data: models.NFSMountLocResponse{
				Address:       "10.0.0.11",
				Nfs:           "10.0.0.11:/backup_share",
				MountLocation: "/mnt/automate",
			},
			NfsMounted:       true,
			ExpectedCheckRes: false,
		},
	}
	for _, e := range tests {
		node := new(models.NFSMountResponse)
		checkShare(e.Data, compareWith, node, e.NfsMounted)
		// checkShare function will append check result in node object.
		isPassed := node.CheckList[0].Passed
		assert.Equal(t, e.ExpectedCheckRes, isPassed)
	}
}

func TestGetResultStructFromRespBody(t *testing.T) {
	tests := []struct {
		TestName     string
		Body         io.Reader
		ExpectedResp *models.NFSMountLocResponse
		ExpectedErr  error
	}{
		{
			TestName:     "Success Response Body",
			Body:         bytes.NewBufferString(SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITH_RESULT_STRUCT),
			ExpectedResp: &SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT,
			ExpectedErr:  nil,
		},
		{
			TestName:     "Failure Response Body",
			Body:         bytes.NewBufferString(FAILED_NFS_MOUNT_LOC_RESPONSE_BODY),
			ExpectedResp: nil,
			ExpectedErr:  errors.New(""),
		},
		{
			TestName:     "Success Response Body (Response Returning string instead of SuccessResponse Object)",
			Body:         bytes.NewBufferString(""),
			ExpectedResp: nil,
			ExpectedErr:  errors.New(""),
		},
		{
			TestName:     "Success Response Body (Doesn’t Contain Result Struct. Instead some other string)",
			Body:         bytes.NewBufferString(SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITHOUT_RESULT_STRUCT),
			ExpectedResp: nil,
			ExpectedErr:  errors.New(""),
		},
	}
	for _, e := range tests {
		res, err := getResultStructFromRespBody(e.Body)
		if e.ExpectedErr != nil {
			assert.Error(t, err)
		}
		assert.Equal(t, res, e.ExpectedResp)

	}
}

func TestTriggerAPI(t *testing.T) {
	ts := mockserver.NewAPI(t).HttpReqMock("/api/v1/fetch/nfs-mount-loc", 200, []byte(SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITH_RESULT_STRUCT), 1*time.Second).Build()
	tests := []struct {
		TestName         string
		URL              string
		ExpectedResponse string
		ExpectedError    error
	}{
		{
			TestName:         "Valid URL with running server",
			URL:              ts.URL,
			ExpectedResponse: SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITH_RESULT_STRUCT,
			ExpectedError:    nil,
		},
		{
			TestName:         "Invalid URL",
			URL:              "http:/whatever.com/",
			ExpectedResponse: "",
			ExpectedError:    errors.New(""),
		},
		{
			TestName:         "Valid URL but no server running there",
			URL:              "http://whatever.com/",
			ExpectedResponse: "",
			ExpectedError:    nil,
		},
	}
	for _, e := range tests {
		resp, err := triggerAPI(e.URL, "/mnt")
		if e.ExpectedError != nil {
			require.Error(t, err)
		} else {
			body, _ := ioutil.ReadAll(resp.Body)
			require.Equal(t, e.ExpectedResponse, string(body))
		}
	}
}

func TestDoAPICall(t *testing.T) {
	ts := mockserver.NewAPI(t).HttpReqMock("/api/v1/fetch/nfs-mount-loc", 200, []byte(SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITH_RESULT_STRUCT), 1*time.Second).Build()
	tests := []struct {
		TestName                     string
		URL                          string
		ChangeURLResponse            bool
		ExpectedCheckListReponsePass bool
		ExpectedError                error
	}{
		{
			TestName:                     "Valid URL with running Server",
			URL:                          ts.URL,
			ChangeURLResponse:            false,
			ExpectedCheckListReponsePass: true,
			ExpectedError:                nil,
		},
		{
			TestName:                     "Invalid URL",
			URL:                          "http:/anything.com",
			ChangeURLResponse:            false,
			ExpectedCheckListReponsePass: false,
			ExpectedError:                errors.New(""),
		},
		{
			TestName:                     "Valid URL but Some Different Response",
			URL:                          ts.URL,
			ChangeURLResponse:            true,
			ExpectedCheckListReponsePass: false,
			ExpectedError:                errors.New(""),
		},
	}

	for index, e := range tests {
		if e.ChangeURLResponse {
			e.URL = mockserver.NewAPI(t).HttpReqMock("/api/v1/fetch/nfs-mount-loc", 200, []byte("Anything Wrong For failing the getResultStructFromRespBody fn call"), 1*time.Second).Build().URL
		}
		shareMap := make(map[string]models.NFSMountLocResponse)
		countMap := make(map[models.NFSMountLocResponse]int)

		resp := doAPICall(e.URL, true, "node_type", "/mount-location", shareMap, "key"+strconv.Itoa(index), countMap)
		if e.ExpectedError != nil {
			assert.Error(t, resp.Error)
		} else {
			assert.Equal(t, resp.CheckList[0].Passed, e.ExpectedCheckListReponsePass)
		}
	}
}

func TestGetNFSMountDetails(t *testing.T) {
	ts := mockserver.NewAPI(t).HttpReqMock("/api/v1/fetch/nfs-mount-loc", 200, []byte(SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITH_RESULT_STRUCT), 1*time.Second).Build()
	ts2 := mockserver.NewAPI(t).HttpReqMock("/api/v1/fetch/nfs-mount-loc", 200, []byte(SUCCESS_NFS_MOUNT_LOC_RESPONSE_BODY_WITH_RESULT_STRUCT), 1*time.Second).Build()

	tests := []struct {
		TestName string
		ReqBody  models.NFSMountRequest
		Response []models.NFSMountResponse
	}{
		{
			TestName: "Giving all services Valid IPs",
			ReqBody: models.NFSMountRequest{
				AutomateNodeIPs:        []string{ts.URL, ts2.URL},
				ChefInfraServerNodeIPs: []string{ts.URL},
				PostgresqlNodeIPs:      []string{ts.URL, ts2.URL},
				OpensearchNodeIPs:      []string{ts.URL},
			},
			Response: []models.NFSMountResponse{
				{IP: ts.URL, NodeType: "automate", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
				{IP: ts2.URL, NodeType: "automate", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
				{IP: ts.URL, NodeType: "chef-infra-server", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
				{IP: ts.URL, NodeType: "postgresql", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
				{IP: ts2.URL, NodeType: "postgresql", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
				{IP: ts.URL, NodeType: "opensearch", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
			},
		},
		{
			TestName: "Giving some Valid And Invalid Ips",
			ReqBody: models.NFSMountRequest{
				AutomateNodeIPs:        []string{ts.URL, "192.168.54.34"},
				ChefInfraServerNodeIPs: []string{ts.URL},
				PostgresqlNodeIPs:      []string{"anything.com", ts2.URL},
				OpensearchNodeIPs:      []string{ts.URL},
			},
			Response: []models.NFSMountResponse{
				{IP: ts.URL, NodeType: "automate", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
				{IP: "192.168.54.34", NodeType: "automate", CheckList: nil, Error: errors.New("")},
				{IP: ts.URL, NodeType: "chef-infra-server", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
				{IP: "anything.com", NodeType: "postgresql", CheckList: nil, Error: errors.New("")},
				{IP: ts2.URL, NodeType: "postgresql", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
				{IP: ts.URL, NodeType: "opensearch", CheckList: []models.Checks{
					{Passed: true},
					{Passed: true},
				}, Error: nil},
			},
		},
	}

	for _, e := range tests {
		nm := NewNFSMountService()
		resp := nm.GetNFSMountDetails(e.ReqBody, true)
		for index, te := range *resp {
			if e.Response[index].Error != nil {
				assert.Error(t, te.Error)
			} else {
				assert.Equal(t, te.CheckList[0].Passed, e.Response[index].CheckList[0].Passed)
				assert.Equal(t, te.CheckList[1].Passed, e.Response[index].CheckList[1].Passed)
			}
			assert.Equal(t, e.Response[index].IP, te.IP)
			assert.Equal(t, e.Response[index].NodeType, te.NodeType)
		}
	}
}

func TestMakeRespBody(t *testing.T) {
	tests := []struct {
		TestName          string
		CountMap          map[models.NFSMountLocResponse]int
		OrderList         []string
		NfsMountResultMap map[string][]models.NFSMountResponse
		ShareMap          map[string]models.NFSMountLocResponse
		RespBodyLen       int
		NfsMounted        bool
		NfsShared         bool
		ExpectedError     error
	}{
		{
			TestName: "NFS is mounted and Shared",
			CountMap: map[models.NFSMountLocResponse]int{
				SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT: 5,
			},
			OrderList: []string{"my-own-key"},
			NfsMountResultMap: map[string][]models.NFSMountResponse{
				"my-own-key": {VALID_NFS_MOUNT_RESPONSE},
			},
			ShareMap: map[string]models.NFSMountLocResponse{
				"my-own-key": SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT,
			},
			RespBodyLen:   1,
			NfsMounted:    true,
			NfsShared:     true,
			ExpectedError: nil,
		},
		{
			TestName: "NFS is mounted but not shared",
			CountMap: map[models.NFSMountLocResponse]int{
				SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT:  5,
				SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT2: 4,
			},
			OrderList: []string{"my-own-key"},
			NfsMountResultMap: map[string][]models.NFSMountResponse{
				"my-own-key": {VALID_NFS_MOUNT_RESPONSE},
			},
			ShareMap: map[string]models.NFSMountLocResponse{
				"my-own-key": SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT2,
			},
			RespBodyLen:   1,
			NfsMounted:    true,
			NfsShared:     false,
			ExpectedError: nil,
		},
		{
			TestName: "NFS is not mounted",
			CountMap: map[models.NFSMountLocResponse]int{
				SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT:  5,
				SUCCESS_NFS_MOUNT_LOC_RESULT_STRUCT2: 4,
				NFS_NOT_MOUNTED_STRUCT:               1,
			},
			OrderList: []string{"my-own-key"},
			NfsMountResultMap: map[string][]models.NFSMountResponse{
				"my-own-key": {VALID_NFS_MOUNT_BUT_NOT_SHARED_RESPONSE},
			},
			ShareMap: map[string]models.NFSMountLocResponse{
				"my-own-key": NFS_NOT_MOUNTED_STRUCT,
			},
			RespBodyLen:   1,
			NfsMounted:    false,
			NfsShared:     false,
			ExpectedError: nil,
		},
	}
	// mp := map[string][]models.NFSMountResponse{"my": {VALID_NFS_MOUNT_BUT_NOT_SHARED_RESPONSE, VALID_NFS_MOUNT_BUT_NOT_SHARED_RESPONSE}}
	// fmt.Println(mp)
	for _, e := range tests {
		respBody := []models.NFSMountResponse{}
		makeRespBody(&respBody, e.CountMap, e.OrderList, e.NfsMountResultMap, e.ShareMap)
		assert.Equal(t, len(respBody), e.RespBodyLen)
		if e.ExpectedError != nil {
			assert.Error(t, respBody[0].Error)
		} else {
			assert.Equal(t, e.NfsMounted, respBody[0].CheckList[0].Passed)
			assert.Equal(t, e.NfsShared, respBody[0].CheckList[1].Passed)
		}
	}
}
