package chef

import (
	"errors"
	"strconv"
)

type UpdatedSinceService struct {
	client *Client
}

// UpdatedSince represents the body of the returned information.
type UpdatedSince struct {
	Action string
	Id     int64
	Path   string
}

// Since gets available cookbook version information.
//
// https://docs.chef.io/api_chef_server/#updated_since
// This end point has long since been deprecated and is no longer available
// Calls will always return 404 not found errors
func (e UpdatedSinceService) Get(sequenceId int64) (updated []UpdatedSince, err error) {
	url := "updated_since?seq=" + strconv.FormatInt(sequenceId, 10)
	err = e.client.magicRequestDecoder("GET", url, nil, &updated)
	if err != nil {
		err = errors.New("Update_since is a deprecated endpoint and always returns 404.")
	}
	return
}
