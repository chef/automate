package integration_test

import (
	"context"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/api/external/secrets"
)

func TestListSecretEmpty(t *testing.T) {
	ctx := context.Background()
	req := new(secrets.Query)

	secrets, err := secretsServer.List(ctx, req)
	assert.NoError(t, err)
	assert.NotNil(t, secrets)
	assert.Equal(t, int32(0), secrets.Total)
}

func TestListSecretFiltersFail(t *testing.T) {
	ctx := context.Background()

	filtersDataCollection := []struct {
		filters []*query.Filter
		message string
	}{
		{
			filters: appendFilters(
				&query.Filter{Key: "ssh", Exclude: true, Values: []string{"tom", "cat"}},
				&query.Filter{Key: "ssh", Exclude: false, Values: []string{"tom", "cat"}},
			),
			message: "Failure because cannot have the same key be excluded and included",
		},
		{
			filters: appendFilters(
				&query.Filter{Key: "invalided_type", Exclude: true, Values: []string{}},
			),
			message: "Failure because not valied type",
		},
	}

	for _, filtersData := range filtersDataCollection {
		req := &secrets.Query{
			Filters: filtersData.filters,
		}

		_, err := secretsServer.List(ctx, req)
		assert.Error(t, err, filtersData.message)
	}
}

func TestListSecretOrder(t *testing.T) {
	ctx := context.Background()

	testDataCollection := []struct {
		order         secrets.Query_OrderType
		sort          string
		inputSecrets  []*secrets.Secret
		outputSecrets []*secrets.Secret
		message       string
	}{
		{
			order: secrets.Query_ASC,
			sort:  "name",
			inputSecrets: []*secrets.Secret{
				{
					Name: "a",
					Type: "ssh",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
				{
					Name: "c",
					Type: "ssh",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
				{
					Name: "b",
					Type: "ssh",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
			},
			outputSecrets: []*secrets.Secret{
				{
					Name: "a",
					Type: "ssh",
				},
				{
					Name: "b",
					Type: "ssh",
				},
				{
					Name: "c",
					Type: "ssh",
				},
			},
			message: "Order asc",
		},
		{
			order: secrets.Query_DESC,
			sort:  "name",
			inputSecrets: []*secrets.Secret{
				{
					Name: "a",
					Type: "ssh",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
				{
					Name: "c",
					Type: "ssh",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
				{
					Name: "b",
					Type: "ssh",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
			},
			outputSecrets: []*secrets.Secret{
				{
					Name: "c",
					Type: "ssh",
				},
				{
					Name: "b",
					Type: "ssh",
				},
				{
					Name: "a",
					Type: "ssh",
				},
			},
			message: "Order name dec",
		},
		{
			order: secrets.Query_DESC,
			sort:  "type",
			inputSecrets: []*secrets.Secret{
				{
					Name: "a",
					Type: "ssh",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
				{
					Name: "c",
					Type: "service_now",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
				{
					Name: "b",
					Type: "winrm",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
			},
			outputSecrets: []*secrets.Secret{
				{
					Name: "b",
					Type: "winrm",
				},
				{
					Name: "a",
					Type: "ssh",
				},
				{
					Name: "c",
					Type: "service_now",
				},
			},
			message: "Order type desc",
		},
		{
			order: secrets.Query_ASC,
			sort:  "type",
			inputSecrets: []*secrets.Secret{
				{
					Name: "a",
					Type: "ssh",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
				{
					Name: "c",
					Type: "service_now",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
				{
					Name: "b",
					Type: "winrm",
					Data: appendKvs(
						&query.Kv{Key: "username", Value: "username"},
						&query.Kv{Key: "password", Value: "password"}),
				},
			},
			outputSecrets: []*secrets.Secret{
				{
					Name: "c",
					Type: "service_now",
				},
				{
					Name: "a",
					Type: "ssh",
				},
				{
					Name: "b",
					Type: "winrm",
				},
			},
			message: "Order type asc",
		},
	}

	for _, testData := range testDataCollection {
		for _, secret := range testData.inputSecrets {
			id, err := secretsServer.Create(ctx, secret)
			assert.NoError(t, err)
			assert.NotNil(t, id)
		}

		req := &secrets.Query{
			Order: testData.order,
			Sort:  testData.sort,
		}

		responseSecrets, err := secretsServer.List(ctx, req)
		assert.NoError(t, err, testData.message)
		assert.NotNil(t, responseSecrets)
		assert.Equal(t, len(testData.outputSecrets), int(responseSecrets.Total), testData.message)

		for index, actualSecret := range responseSecrets.Secrets {
			expectedSecret := testData.outputSecrets[index]
			assert.Equal(t, actualSecret.Name, expectedSecret.Name, testData.message)
			assert.Equal(t, actualSecret.Type, expectedSecret.Type, testData.message)
		}

		deleteAllSecrets()
	}
}

func TestListSecretPaging(t *testing.T) {
	ctx := context.Background()
	totalAmount := 10
	perPage := 1
	originalSecrets := make([]*secrets.Secret, totalAmount)
	for index := 0; index < totalAmount; index++ {
		secret := &secrets.Secret{
			Name: strconv.Itoa(index),
			Type: "ssh",
			Data: appendKvs(&query.Kv{Key: "username", Value: "username"},
				&query.Kv{Key: "password", Value: "password"}),
		}
		id, err := secretsServer.Create(ctx, secret)
		assert.NoError(t, err)
		assert.NotNil(t, id)

		originalSecrets[index] = secret
	}

	for page := 0; page < totalAmount/perPage; page++ {
		req := &secrets.Query{
			Order:   secrets.Query_ASC,
			Sort:    "name",
			Page:    int32(page + 1),
			PerPage: int32(perPage),
		}

		responseSecrets, err := secretsServer.List(ctx, req)
		assert.NoError(t, err)
		assert.NotNil(t, responseSecrets)
		assert.Equal(t, perPage, len(responseSecrets.Secrets))
		assert.Equal(t, totalAmount, int(responseSecrets.Total))

		for index, actualSecret := range responseSecrets.Secrets {
			expectedSecret := originalSecrets[index+page*perPage]
			assert.Equal(t, expectedSecret.Name, actualSecret.Name)
		}
	}

	deleteAllSecrets()
}

func TestListSecretFilterByName(t *testing.T) {
	ctx := context.Background()

	testData := struct {
		order         secrets.Query_OrderType
		sort          string
		inputSecrets  []*secrets.Secret
		outputSecrets []*secrets.Secret
		message       string
	}{

		order: secrets.Query_ASC,
		sort:  "name",
		inputSecrets: []*secrets.Secret{
			{
				Name: "data feed secret",
				Type: "data_feed",
				Data: appendKvs(
					&query.Kv{Key: "username", Value: "username"},
					&query.Kv{Key: "password", Value: "password"}),
			},
			{
				Name: "service now secret",
				Type: "service_now",
				Data: appendKvs(
					&query.Kv{Key: "username", Value: "username"},
					&query.Kv{Key: "password", Value: "password"}),
			},
		},
		outputSecrets: []*secrets.Secret{
			{
				Name: "data feed secret",
				Type: "data_feed",
			},
		},
		message: "Order asc",
	}

	for _, secret := range testData.inputSecrets {
		id, err := secretsServer.Create(ctx, secret)
		assert.NoError(t, err)
		assert.NotNil(t, id)
	}

	nameFilter := &query.Filter{
		Key:    "name",
		Values: []string{"data feed secret"},
	}
	req := &secrets.Query{
		Order:   testData.order,
		Sort:    testData.sort,
		Filters: []*query.Filter{nameFilter},
	}

	responseSecrets, err := secretsServer.List(ctx, req)
	assert.NoError(t, err, testData.message)
	assert.NotNil(t, responseSecrets)
	assert.Equal(t, len(testData.outputSecrets), int(responseSecrets.Total), testData.message)

	for index, actualSecret := range responseSecrets.Secrets {
		expectedSecret := testData.outputSecrets[index]
		assert.Equal(t, actualSecret.Name, expectedSecret.Name, testData.message)
		assert.Equal(t, actualSecret.Type, expectedSecret.Type, testData.message)
	}

	deleteAllSecrets()

}


func TestListSecretFilterByType(t *testing.T) {
	ctx := context.Background()

	testData := struct {
		order         secrets.Query_OrderType
		sort          string
		inputSecrets  []*secrets.Secret
		outputSecrets []*secrets.Secret
		message       string
	}{

		order: secrets.Query_ASC,
		sort:  "name",
		inputSecrets: []*secrets.Secret{
			{
				Name: "data feed secret",
				Type: "data_feed",
				Data: appendKvs(
					&query.Kv{Key: "username", Value: "username"},
					&query.Kv{Key: "password", Value: "password"}),
			},
			{
				Name: "service now secret",
				Type: "service_now",
				Data: appendKvs(
					&query.Kv{Key: "username", Value: "username"},
					&query.Kv{Key: "password", Value: "password"}),
			},
		},
		outputSecrets: []*secrets.Secret{
			{
				Name: "data feed secret",
				Type: "data_feed",
			},
		},
		message: "Order asc",
	}

	for _, secret := range testData.inputSecrets {
		id, err := secretsServer.Create(ctx, secret)
		assert.NoError(t, err)
		assert.NotNil(t, id)
	}

	typeFilter := &query.Filter{
		Key:    "type",
		Values: []string{"data_feed"},
	}
	req := &secrets.Query{
		Order:   testData.order,
		Sort:    testData.sort,
		Filters: []*query.Filter{typeFilter},
	}

	responseSecrets, err := secretsServer.List(ctx, req)
	assert.NoError(t, err, testData.message)
	assert.NotNil(t, responseSecrets)
	assert.Equal(t, len(testData.outputSecrets), int(responseSecrets.Total), testData.message)

	for index, actualSecret := range responseSecrets.Secrets {
		expectedSecret := testData.outputSecrets[index]
		assert.Equal(t, actualSecret.Name, expectedSecret.Name, testData.message)
		assert.Equal(t, actualSecret.Type, expectedSecret.Type, testData.message)
	}

	deleteAllSecrets()

}
