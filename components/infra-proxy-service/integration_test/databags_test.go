package integration_test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	"github.com/stretchr/testify/assert"
	"google.golang.org/protobuf/types/known/structpb"
)

func TestGetDatabags(t *testing.T) {
	// Pre-populated that has been added by scripts
	// Validating the data bags search based on these records.
	// rpc GetDataBags (request.DataBags) returns (response.DataBags)
	ctx := context.Background()
	req := &request.DataBags{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
	}

	res, err := infraProxy.GetDataBags(ctx, req)
	assert.NoError(t, err)
	existingTotal := len(res.DataBags)

	// Add a data bag
	dataBagName := fmt.Sprintf("data-bag-%d", time.Now().Nanosecond())
	dataBag, err := infraProxy.CreateDataBag(ctx, &request.CreateDataBag{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     dataBagName,
	})
	assert.NoError(t, err)
	assert.NotNil(t, dataBag)

	fetchRes, err := infraProxy.GetDataBags(ctx, req)
	assert.Equal(t, existingTotal+1, len(fetchRes.DataBags))
}

func TestGetDatabagItems(t *testing.T) {
	ctx := context.Background()

	// Add a data bag
	dataBagName := fmt.Sprintf("data-bag-%d", time.Now().Nanosecond())
	dataBag, err := infraProxy.CreateDataBag(ctx, &request.CreateDataBag{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     dataBagName,
	})
	assert.NoError(t, err)
	assert.NotNil(t, dataBag)

	req := &request.DataBagItems{
		ServerId: autoDeployedChefServerID,
		OrgId:    autoDeployedChefOrganizationID,
		Name:     dataBagName,
	}

	t.Run("when the data bag is having no items", func(t *testing.T) {
		res, err := infraProxy.GetDataBagItems(ctx, req)
		assert.NoError(t, err)
		assert.Equal(t, 0, len(res.GetItems()))
		assert.Equal(t, dataBagName, res.GetName())
		assert.Equal(t, 0, int(res.GetPage()))
		assert.Equal(t, 0, int(res.GetTotal()))
	})

	//Adds data bag items records
	addDataBagItems(ctx, dataBagName, 10)

	t.Run("items list with a per_page search param", func(t *testing.T) {
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
		req.SearchQuery = &request.SearchQuery{
			Q:       "INVALID_QUERY",
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

// Adds data bag items records
func addDataBagItems(ctx context.Context, dabaBagName string, n int) int {
	total := 0
	for i := 0; i < n; i++ {
		itemId := fmt.Sprintf("item-%d", time.Now().Nanosecond())
		req := &request.CreateDataBagItem{
			ServerId: autoDeployedChefServerID,
			OrgId:    autoDeployedChefOrganizationID,
			Name:     dabaBagName,
			Data: &structpb.Struct{
				Fields: map[string]*structpb.Value{
					"id": &structpb.Value{Kind: &structpb.Value_StringValue{StringValue: itemId}},
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
