package chef

import (
	"errors"
	"fmt"
	"strings"
)

type SearchService struct {
	client *Client
}

// SearchQuery Is the struct for holding a query request
type SearchQuery struct {
	// The index you want to search
	Index string

	// The query you want to execute. This is the 'chef' query ex: 'chef_environment:prod'
	Query string

	// Sort order you want the search results returned
	SortBy string

	// Starting position for search
	Start int

	// Number of rows to return
	Rows int
}

// String implements the Stringer Interface for the SearchQuery
func (q SearchQuery) String() string {
	return fmt.Sprintf("%s?q=%s&rows=%d&sort=%s&start=%d", q.Index, q.Query, q.Rows, q.SortBy, q.Start)
}

// SearchResult will return a slice of interface{} of chef-like objects (roles/nodes/etc)
type SearchResult struct {
	Total int
	Start int
	Rows  []interface{}
}

// Do will execute the search query on the client
func (q SearchQuery) Do(client *Client) (res SearchResult, err error) {
	fullUrl := fmt.Sprintf("search/%s", q)
	err = client.magicRequestDecoder("GET", fullUrl, nil, &res)
	return
}

// DoPartial will execute the search query on the client with partial mapping
func (q SearchQuery) DoPartial(client *Client, params map[string]interface{}) (res SearchResult, err error) {
	fullUrl := fmt.Sprintf("search/%s", q)

	body, err := JSONReader(params)
	if err != nil {
		debug("Problem encoding params for body %v", err.Error())
		return
	}

	err = client.magicRequestDecoder("POST", fullUrl, body, &res)
	return
}

// NewSearch is a constructor for a SearchQuery struct. This is used by other search service methods to perform search requests on the server
func (e SearchService) NewQuery(idx, statement string) (query SearchQuery, err error) {
	// validate statement
	if !strings.Contains(statement, ":") {
		err = errors.New("statement is malformed")
		return
	}

	query = SearchQuery{
		Index: idx,
		Query: statement,
		// These are the defaults in chef: https://github.com/opscode/chef/blob/master/lib/chef/search/query.rb#L102-L105
		SortBy: "X_CHEF_id_CHEF_X asc",
		Start:  0,
		Rows:   1000,
	}

	return
}

// Exec runs the query on the index passed in. This is a helper method. If you want more control over the query  use NewQuery and its Do() method.
// BUG(spheromak): Should we use Exec or SearchQuery.Do() or have both ?
func (e SearchService) Exec(idx, statement string) (res SearchResult, err error) {
	//  Copy-paste here till We decide which way to go with Exec vs Do
	if !strings.Contains(statement, ":") {
		err = errors.New("statement is malformed")
		return
	}

	query := SearchQuery{
		Index: idx,
		Query: statement,
		// These are the defaults in chef: https://github.com/opscode/chef/blob/master/lib/chef/search/query.rb#L102-L105
		SortBy: "X_CHEF_id_CHEF_X asc",
		Start:  0,
		Rows:   1000,
	}

	res, err = query.Do(e.client)
	start := res.Start
	inc := 1000
	total := res.Total

	for start+inc <= total {
		query.Start = query.Start + 1000
		start = query.Start
		ares, err := query.Do(e.client)
		if err != nil {
			return res, err
		}
		res.Rows = append(res.Rows, ares.Rows...)
	}
	return
}

// PartialExec Executes a partial search based on passed in params and the query.
func (e SearchService) PartialExec(idx, statement string, params map[string]interface{}) (res SearchResult, err error) {
	query := SearchQuery{
		Index: idx,
		Query: statement,
		// These are the defaults in chef: https://github.com/opscode/chef/blob/master/lib/chef/search/query.rb#L102-L105
		SortBy: "X_CHEF_id_CHEF_X asc",
		Start:  0,
		Rows:   1000,
	}

	fullUrl := fmt.Sprintf("search/%s", query)

	body, err := JSONReader(params)
	if err != nil {
		debug("Problem encoding params for body")
		return
	}

	err = e.client.magicRequestDecoder("POST", fullUrl, body, &res)
	if err != nil {
		return
	}

	// the total rows available for this query across all pages
	total := res.Total
	// the maximum number of rows in each page
	inc := query.Rows
	paged_res := SearchResult{}

	for start := res.Start; start+inc <= total; start += inc {
		query.Start = start + inc
		fullUrl = fmt.Sprintf("search/%s", query)

		err = e.client.magicRequestDecoder("POST", fullUrl, body, &paged_res)
		if err != nil {
			return
		}
		// accumulate this page of results into the primary SearchResult instance
		res.Rows = append(res.Rows, paged_res.Rows...)
	}
	return
}

// List lists the nodes in the Chef server.
//
// Chef API docs: http://docs.opscode.com/api_chef_server.html#id25
func (e SearchService) Indexes() (data map[string]string, err error) {
	err = e.client.magicRequestDecoder("GET", "search", nil, &data)
	return
}
