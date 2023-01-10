package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	"google.golang.org/protobuf/types/known/structpb"
)

func TestGetDatabags(t *testing.T) {
	// Pre-populated that has been added by scripts
	// Validating the data bags search based on these records.
	// rpc GetDataBags (request.DataBags) returns (response.DataBags)
	req := &request.DataBags{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
	}
	res, err := infraProxy.GetDataBags(ctx, req)
	assert.NoError(t, err)
	existingTotal := len(res.DataBags)

	// Add a data bag
	dataBagName := fmt.Sprintf("data-bag-test-%d", time.Now().Nanosecond())
	dataBag, err := infraProxy.CreateDataBag(ctx, &request.CreateDataBag{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     dataBagName,
	})
	assert.NoError(t, err)
	assert.NotNil(t, dataBag)

	fetchRes, err := infraProxy.GetDataBags(ctx, req)
	assert.NoError(t, err)
	assert.Equal(t, existingTotal+1, len(fetchRes.DataBags))
}

func TestGetDatabagItems(t *testing.T) {

	t.Run("when the data bag is having no items", func(t *testing.T) {
		req, dataBagName := createDataBag(1)
		res, err := infraProxy.GetDataBagItems(ctx, req)
		assert.NoError(t, err)
		assert.Equal(t, 0, len(res.GetItems()))
		assert.Equal(t, dataBagName, res.GetName())
		assert.Equal(t, 0, int(res.GetPage()))
		assert.Equal(t, 0, int(res.GetTotal()))
	})

	t.Run("items list with a per_page search param", func(t *testing.T) {
		req, dataBagName := createDataBag(2)
		itemId := fmt.Sprintf("item-%d", time.Now().Nanosecond())
		createReq := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     dataBagName,
			Data: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"id":   &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: itemId}},
					"key1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: fmt.Sprintf("key1-%d", time.Now().Nanosecond())}},
					"key2": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: fmt.Sprintf("key2-%d", time.Now().Nanosecond())}},
				},
			},
		}
		dbItem, err := infraProxy.CreateDataBagItem(ctx, createReq)
		assert.NoError(t, err)
		assert.NotNil(t, dbItem)
		time.Sleep(5 * time.Second)

		req.SearchQuery = &request.SearchQuery{
			PerPage: 1,
		}

		res, err := infraProxy.GetDataBagItems(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.GetPage()))
		assert.Equal(t, 1, len(res.GetItems()))
		assert.GreaterOrEqual(t, int(res.Total), 0)
	})

	t.Run("items list with a page search param", func(t *testing.T) {
		req, dataBagName := createDataBag(3)
		time.Sleep(2 * time.Second)
		itemId1 := fmt.Sprintf("item-%d", time.Now().Nanosecond())
		createReq1 := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     dataBagName,
			Data: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"id": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: itemId1}},
				},
			},
		}
		dbItem1, err := infraProxy.CreateDataBagItem(ctx, createReq1)
		assert.NoError(t, err)
		assert.NotNil(t, dbItem1)
		time.Sleep(2 * time.Second)

		itemId2 := fmt.Sprintf("item-%d", time.Now().Nanosecond())
		createReq2 := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     dataBagName,
			Data: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"id": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: itemId2}},
				},
			},
		}
		dbItem2, err := infraProxy.CreateDataBagItem(ctx, createReq2)
		assert.NoError(t, err)
		assert.NotNil(t, dbItem2)
		time.Sleep(2 * time.Second)

		req.SearchQuery = &request.SearchQuery{
			PerPage: 1,
			Page:    1,
		}

		res, err := infraProxy.GetDataBagItems(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 1, int(res.GetPage()))
		assert.Equal(t, 1, len(res.GetItems()))
		assert.GreaterOrEqual(t, int(res.Total), 0)
	})

	t.Run("items list with an invalid query search param", func(t *testing.T) {
		req, _ := createDataBag(4)
		req.SearchQuery = &request.SearchQuery{
			Q:       "NO_KEY:NO_VALUE",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetDataBagItems(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, 0, int(res.GetPage()))
		assert.Equal(t, 0, int(res.GetTotal()))
		assert.Equal(t, 0, len(res.GetItems()))
	})

	t.Run("items list with an invalid query search param", func(t *testing.T) {
		req, _ := createDataBag(5)
		req.SearchQuery = &request.SearchQuery{
			Q:       "INVALID_QUERY:INVALID_QUERY",
			Page:    0,
			PerPage: 5,
		}
		res, err := infraProxy.GetDataBagItems(ctx, req)
		assert.NoError(t, err)
		assert.Equal(t, 0, int(res.GetPage()))
		assert.Equal(t, 0, int(res.GetTotal()))
		assert.Equal(t, 0, len(res.GetItems()))
	})
}

func TestCreateDatabag(t *testing.T) {
	ctx := context.Background()
	t.Run("when a valid data bag is submitted, creates the new data bag successfully", func(t *testing.T) {
		name := fmt.Sprintf("data-bag-create-test-%d", time.Now().Nanosecond())
		req := &request.CreateDataBag{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		res, err := infraProxy.CreateDataBag(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, name, res.GetName())
	})

	t.Run("when the data bag already exists, raise the error data bag already exists", func(t *testing.T) {
		name := fmt.Sprintf("data-bag-create-test-1-%d", time.Now().Nanosecond())
		req := &request.CreateDataBag{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		res, err := infraProxy.CreateDataBag(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)

		nextRes, err := infraProxy.CreateDataBag(ctx, req)
		assert.Nil(t, nextRes)
		assert.Error(t, err, "Data bag already exists")
		grpctest.AssertCode(t, codes.AlreadyExists, err)
	})

	t.Run("when the data bag required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
		req := &request.CreateDataBag{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
		}
		res, err := infraProxy.CreateDataBag(ctx, req)
		assert.Nil(t, res)
		assert.Error(t, err, "must supply data bag name")
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})
}

func TestCreateDatabagItem(t *testing.T) {
	ctx := context.Background()
	t.Run("when a valid data bag item is submitted, creates the new data bag item successfully", func(t *testing.T) {
		name := fmt.Sprintf("data-bag-create-databag-item-%d", time.Now().Nanosecond())
		dataReq := &request.CreateDataBag{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		data, err := infraProxy.CreateDataBag(ctx, dataReq)
		assert.NoError(t, err)
		assert.NotNil(t, data)

		item := fmt.Sprintf("data-bag-item-%d", time.Now().Nanosecond())
		req := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Data: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"id": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: item}},
				},
			},
		}
		res, err := infraProxy.CreateDataBagItem(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)
		assert.Equal(t, name, res.GetName())
		assert.Equal(t, item, res.GetId())
	})

	t.Run("when the data bag item already exists, raise the error data bag item already exists", func(t *testing.T) {
		name := fmt.Sprintf("data-bag-bag-create-databag-item-1-%d", time.Now().Nanosecond())
		dataReq := &request.CreateDataBag{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
		}
		data, err := infraProxy.CreateDataBag(ctx, dataReq)
		assert.NoError(t, err)
		assert.NotNil(t, data)

		item := fmt.Sprintf("data-bag-item-%d", time.Now().Nanosecond())
		req := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     name,
			Data: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"id": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: item}},
				},
			},
		}
		res, err := infraProxy.CreateDataBagItem(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, res)

		nextRes, err := infraProxy.CreateDataBagItem(ctx, req)
		assert.Nil(t, nextRes)
		assert.Error(t, err, "Data bag item already exists")
		grpctest.AssertCode(t, codes.AlreadyExists, err)
	})

	t.Run("when the data bag required field name is missing or empty, raise an invalid argument error", func(t *testing.T) {
		req := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
		}
		res, err := infraProxy.CreateDataBagItem(ctx, req)
		assert.Nil(t, res)
		assert.Error(t, err, "must supply data bag name")
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})

	t.Run("when the data bag required field item is missing or empty, raise an invalid argument error", func(t *testing.T) {
		req := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     fmt.Sprintf("data-bag-bag-create-databag-item-2-%d", time.Now().Nanosecond()),
		}
		res, err := infraProxy.CreateDataBagItem(ctx, req)
		assert.Nil(t, res)
		errMsg := "databag item is required and must contain at least one non-whitespace character"
		assert.Error(t, err, "must supply data bag item")
		require.Contains(t, err.Error(), errMsg)
		grpctest.AssertCode(t, codes.InvalidArgument, err)

		//Data contains no id
		req.Data = &structpb.Struct{
			Fields: map[string]*structpb.Value{},
		}
		res1, err := infraProxy.CreateDataBagItem(ctx, req)
		assert.Nil(t, res1)
		assert.Error(t, err, "must supply data bag item")
		require.Contains(t, err.Error(), errMsg)
		grpctest.AssertCode(t, codes.InvalidArgument, err)

		//Data contains item id attribute with empty string
		req.Data = &structpb.Struct{
			Fields: map[string]*structpb.Value{
				"id": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: ""}},
			},
		}
		res2, err := infraProxy.CreateDataBagItem(ctx, req)
		assert.Nil(t, res2)
		assert.Error(t, err, "must supply data bag item")
		require.Contains(t, err.Error(), errMsg)
		grpctest.AssertCode(t, codes.InvalidArgument, err)
	})
}

// Adds data bag with items records
func addDatabagsWithItems(n int) int {
	total := 0
	for i := 0; i < 10; i++ {
		dataBagName := fmt.Sprintf("data-bag-%d", time.Now().Nanosecond())
		_, err := infraProxy.CreateDataBag(ctx, &request.CreateDataBag{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     dataBagName,
		})

		if err == nil {
			addDataBagItems(dataBagName, n)
			total++
		}
	}

	return total
}

// Adds data bag items records
func addDataBagItems(dabaBagName string, n int) int {
	total := 0
	for i := 0; i < n; i++ {
		itemId := fmt.Sprintf("item-%d", time.Now().Nanosecond())
		req := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     dabaBagName,
			Data: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"id":   &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: itemId}},
					"key1": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: fmt.Sprintf("key1-%d", time.Now().Nanosecond())}},
					"key2": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: fmt.Sprintf("key2-%d", time.Now().Nanosecond())}},
				},
			},
		}
		_, err := infraProxy.CreateDataBagItem(ctx, req)

		if err == nil {
			total++
		}
	}

	return total
}

// create data bag items for test scenario
func createDataBag(n int) (*request.DataBagItems, string) {
	dataBagName := fmt.Sprintf("data-bag-%d-test-%d", n, time.Now().Nanosecond())
	_, err := infraProxy.CreateDataBag(ctx, &request.CreateDataBag{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     dataBagName,
	})
	if err != nil {
		return nil, dataBagName
	}

	req := &request.DataBagItems{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     dataBagName,
	}

	return req, dataBagName
}
