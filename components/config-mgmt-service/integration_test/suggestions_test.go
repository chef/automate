//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package integration_test

import (
	"context"
	"fmt"
	"strconv"
	"testing"

	"google.golang.org/grpc/codes"

	gp "github.com/golang/protobuf/ptypes/struct"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/cfgmgmt/request"
	authzConstants "github.com/chef/automate/components/authz-service/constants/v2"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestSuggestionsEmptyRequestReturnsError(t *testing.T) {
	ctx := context.Background()
	req := request.Suggestion{}

	res, err := cfgmgmt.GetSuggestions(ctx, &req)
	grpctest.AssertCode(t, codes.InvalidArgument, err)
	assert.Nil(t, res)
}

func TestSuggestionsWithAnInvalidTypeReturnsError(t *testing.T) {
	ctx := context.Background()
	cases := []string{"fake", "my-platform", "PolicyName", "something_else"}

	for _, cType := range cases {
		t.Run(fmt.Sprintf("with type '%v' it should throw error", cType), func(t *testing.T) {
			req := request.Suggestion{Type: cType}
			res, err := cfgmgmt.GetSuggestions(ctx, &req)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, res)
		})
	}
}

func TestSuggestionsLargeArrayValues(t *testing.T) {

	ctx := context.Background()
	terms := make([]string, 105)
	for index := 0; index < 105; index++ {
		terms[index] = "a_" + strconv.Itoa(index)
	}

	cases := []struct {
		description string
		nodes       []iBackend.Node
		request     request.Suggestion
		expected    []string
	}{
		{
			description: "over 105 attributes",
			nodes: []iBackend.Node{
				{
					Attributes: append(terms, "zum.epel-debuginfo.gpgcheck"),
				},
			},
			request: request.Suggestion{
				Type: "attribute",
				Text: "zum.epel-debuginfo.gpgcheck",
			},
			expected: []string{"zum.epel-debuginfo.gpgcheck"},
		},
		{
			description: "over 105 cookbooks",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						Cookbooks: append(terms, "zum.epel-debuginfo.gpgcheck"),
					},
				},
			},
			request: request.Suggestion{
				Type: "cookbook",
				Text: "zum.epel-debuginfo.gpgcheck",
			},
			expected: []string{"zum.epel-debuginfo.gpgcheck"},
		},
		{
			description: "case sensitive 1",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						Cookbooks: append(terms, "ZZZ.ZZZ-ZZzzzZZZ"),
					},
				},
			},
			request: request.Suggestion{
				Type: "cookbook",
				Text: "zzz.zzz-zzzzzzzz",
			},
			expected: []string{"zzz.zzz-zzzzzzzz"},
		},
		{
			description: "case sensitive 2",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						Cookbooks: append(terms, "ZZZ.ZZZ-ZZzzzZZZ"),
					},
				},
			},
			request: request.Suggestion{
				Type: "cookbook",
				Text: "ZZZ.ZZZ-ZZzzzZZZ",
			},
			expected: []string{"zzz.zzz-zzzzzzzz"},
		},
		{
			description: "case sensitive 3",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						Cookbooks: append(terms, "zzz.zzz-zzzzzzzz"),
					},
				},
			},
			request: request.Suggestion{
				Type: "cookbook",
				Text: "ZZZ.ZZZ-ZZzzzZZZ",
			},
			expected: []string{"zzz.zzz-zzzzzzzz"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Many items: %s", test.description), func(t *testing.T) {
			// Adding required node data
			for index := range test.nodes {
				test.nodes[index].Exists = true
				test.nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			suite.IngestNodes(test.nodes)
			defer suite.DeleteAllDocuments()
			res, err := cfgmgmt.GetSuggestions(ctx, &test.request)
			assert.Nil(t, err)

			actualSuggestions := extractTextFromSuggestionsResponse(res)

			assert.ElementsMatch(t, test.expected, actualSuggestions)
		})
	}
}

// If there are over one hundred and five attributes that start with the prefix 'zum' and one term 'zum'.
// Test if the term 'zum' returned when the text 'zum' is requested.
func TestSuggestionsLargeCollectionOfSamePrefixTerm(t *testing.T) {
	terms := make([]string, 110)
	for index := 0; index < 110; index++ {
		terms[index] = "zum_" + strconv.Itoa(index)
	}
	terms = append(terms, "zum")

	node := iBackend.Node{
		Exists:     true,
		Attributes: terms,
		NodeInfo: iBackend.NodeInfo{
			EntityUuid: newUUID(),
		},
	}

	suite.IngestNodes([]iBackend.Node{node})
	defer suite.DeleteAllDocuments()

	cases := []struct {
		description  string
		request      request.Suggestion
		expectedTerm string
	}{
		{
			description: "suggest term 'zum'",
			request: request.Suggestion{
				Type: "attribute",
				Text: "zum",
			},
			expectedTerm: "zum",
		},
		{
			description: "suggest term 'zum_0'",
			request: request.Suggestion{
				Type: "attribute",
				Text: "zum_0",
			},
			expectedTerm: "zum_0",
		},
		{
			description: "suggest term 'zum_99'",
			request: request.Suggestion{
				Type: "attribute",
				Text: "zum_99",
			},
			expectedTerm: "zum_99",
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Find term: %s", test.description), func(t *testing.T) {
			res, err := cfgmgmt.GetSuggestions(context.Background(), &test.request)
			assert.Nil(t, err)

			actualSuggestions := extractTextFromSuggestionsResponse(res)

			assert.Contains(t, actualSuggestions, test.expectedTerm)
		})
	}
}

func TestSuggestionsFiltered(t *testing.T) {

	ctx := context.Background()

	cases := []struct {
		description string
		nodes       []iBackend.Node
		request     request.Suggestion
		expected    []string
	}{
		{
			description: "All node names suggestions are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "3",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "4",
					},
				},
			},
			request: request.Suggestion{
				Type: "name",
			},
			expected: []string{"1", "2", "3", "4"},
		},
		{
			description: "All node name suggestions are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "3",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "4",
					},
				},
			},
			request: request.Suggestion{
				Type: "name",
			},
			expected: []string{"1", "2", "3", "4"},
		},
		{
			description: "Only node name suggestions for nodes that have environment 'a' are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "1",
						Environment: "a",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "2",
						Environment: "a",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "3",
						Environment: "b",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "4",
						Environment: "b",
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"environment:a"},
			},
			expected: []string{"1", "2"},
		},
		{
			description: "Only node name suggestions for nodes that have environment 'a' and platform 'ubuntu' are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "1",
						Environment: "a",
						Platform:    "ubuntu",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "2",
						Environment: "a",
						Platform:    "debian",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "3",
						Environment: "b",
						Platform:    "windows",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "4",
						Environment: "b",
						Platform:    "windows",
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"environment:a", "platform:ubuntu"},
			},
			expected: []string{"1"},
		},
		{
			description: "Only node name suggestions for nodes that have environments that start with 'a-' are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "1",
						Environment: "a-dev",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "2",
						Environment: "a-prod",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "3",
						Environment: "b",
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"environment:a-*"},
			},
			expected: []string{"1", "2"},
		},
		{
			description: "Only node name suggestions for nodes that have the 'nginx' cookbook are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "1",
						Cookbooks: []string{"nginx", "mariadb"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "2",
						Cookbooks: []string{"nginx"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "3",
						Cookbooks: []string{"apache", "mariadb"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "4",
						Cookbooks: []string{"apache", "wordpress"},
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"cookbook:nginx"},
			},
			expected: []string{"1", "2"},
		},
		{
			description: "All node name suggestions should be returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a2-dev",
						Cookbooks: []string{"nginx", "mariadb"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a2-prod",
						Cookbooks: []string{"nginx"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a1-airgap",
						Cookbooks: []string{"apache", "mariadb"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a1-workflow",
						Cookbooks: []string{"apache", "wordpress"},
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"name:a2-*"},
			},
			expected: []string{"a2-dev", "a2-prod", "a1-airgap", "a1-workflow"},
		},
		{
			description: "Only node name suggestions for nodes that have a 'california' attribute, are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
					},
					Attributes: []string{"california", "m4"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
					},
					Attributes: []string{"california"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "3",
					},
					Attributes: []string{"ohio", "m2"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "4",
					},
					Attributes: []string{"Florida", "m7"},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"attribute:california"},
			},
			expected: []string{"1", "2"},
		},
		{
			description: "Only node name suggestions for nodes that have an 'v2' chef tag, are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						ChefTags: []string{"v2", "dev"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						ChefTags: []string{"v2"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "3",
						ChefTags: []string{"v1", "prod"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "4",
						ChefTags: []string{"v3", "dev"},
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"chef_tags:v2"},
			},
			expected: []string{"1", "2"},
		},
		{
			description: "Only node name suggestions for nodes that have an 'v2' Chef Client Version, are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "1",
						ChefVersion: "v2",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "2",
						ChefVersion: "v2",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "3",
						ChefVersion: "v1",
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:    "4",
						ChefVersion: "v3",
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"chef_version:v2"},
			},
			expected: []string{"1", "2"},
		},
		{
			description: "Only suggestions for cookbooks from nodes that have names that start with 'a2-' are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a2-dev",
						Cookbooks: []string{"nginx", "mariadb"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a2-prod",
						Cookbooks: []string{"nginx"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a1-airgap",
						Cookbooks: []string{"apache", "mariadb"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:  "a1-workflow",
						Cookbooks: []string{"apache", "wordpress"},
					},
				},
			},
			request: request.Suggestion{
				Type:   "cookbook",
				Filter: []string{"name:a2-*"},
			},
			expected: []string{"nginx", "mariadb"},
		},
		{
			description: "Only node name suggestions for nodes that have the 'nginx' recipe are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						Recipes:  []string{"nginx", "mariadb"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						Recipes:  []string{"nginx"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "3",
						Recipes:  []string{"apache", "mariadb"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "4",
						Recipes:  []string{"apache", "wordpress"},
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"recipe:nginx"},
			},
			expected: []string{"1", "2"},
		},
		{
			description: "Only node name suggestions for nodes that have the 'a' resource name are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "1",
						ResourceNames: []string{"a", "b"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "2",
						ResourceNames: []string{"a"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "3",
						ResourceNames: []string{"c", "b"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName:      "4",
						ResourceNames: []string{"d", "e"},
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"resource_name:a"},
			},
			expected: []string{"1", "2"},
		},
		{
			description: "Only node name suggestions for nodes that have the 'a' role are returned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "1",
						Roles:    []string{"a", "b"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "2",
						Roles:    []string{"a"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "3",
						Roles:    []string{"c", "b"},
					},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "4",
						Roles:    []string{"d", "e"},
					},
				},
			},
			request: request.Suggestion{
				Type:   "name",
				Filter: []string{"role:a"},
			},
			expected: []string{"1", "2"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Narrowing: %s", test.description), func(t *testing.T) {
			// Adding required node data
			for index := range test.nodes {
				test.nodes[index].Exists = true
				test.nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			// Add node with project
			suite.IngestNodes(test.nodes)
			defer suite.DeleteAllDocuments()
			res, err := cfgmgmt.GetSuggestions(ctx, &test.request)
			assert.Nil(t, err)

			actualSuggestions := extractTextFromSuggestionsResponse(res)

			assert.ElementsMatch(t, test.expected, actualSuggestions)
		})
	}

}

func TestSuggestionsEmptyErrorMessage(t *testing.T) {
	nodes := []iBackend.Node{
		{
			NodeInfo: iBackend.NodeInfo{
				NodeName: "1",
			},
			ErrorMessage: "", //nonvalid suggestion 1"
		},
		{
			NodeInfo: iBackend.NodeInfo{
				NodeName: "2",
			},
			ErrorMessage: "", //nonvalid suggestion 2"
		},
		{
			NodeInfo: iBackend.NodeInfo{
				NodeName: "3",
			},
			ErrorMessage: "valid suggestion 1",
		},
		{
			NodeInfo: iBackend.NodeInfo{
				NodeName: "4",
			},
			ErrorMessage: "valid suggestion 2",
		},
	}

	request := request.Suggestion{
		Type: "error",
		Text: "v",
	}

	expected := []string{"valid suggestion 1", "valid suggestion 2"}

	// Adding required node data
	for index := range nodes {
		nodes[index].Exists = true
		nodes[index].NodeInfo.EntityUuid = newUUID()
	}

	// Add node with project
	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()
	res, err := cfgmgmt.GetSuggestions(context.Background(), &request)
	assert.Nil(t, err)

	actualSuggestions := extractTextFromSuggestionsResponse(res)

	assert.ElementsMatch(t, expected, actualSuggestions)
}

func TestSuggestionsWithTableDriven(t *testing.T) {
	dataNodes := []struct {
		number     int
		node       iBackend.NodeInfo
		attributes []string
		exists     bool
		namePrefix string
	}{
		{10, iBackend.NodeInfo{
			Status: "success", Platform: "windows",
			PolicyGroup: "heros", PolicyRevision: "marvel", PolicyName: "comics",
			Cookbooks: []string{"avengers", "justice_league", "guardians_of_the_galaxy"},
			Recipes:   []string{"thor", "ironman", "ant-man", "spiderman", "dr_strange"},
			Roles:     []string{"heros", "villains"}, ResourceNames: []string{"super_powers"},
			Environment: "games", OrganizationName: "org2", ChefVersion: "1.0",
			ChefTags: []string{"chef"}},
			[]string{"can_fly", "magic", "god", "immortal", "complex_attr_lala"},
			true,
			"node-"},
		{10, iBackend.NodeInfo{
			Status: "success", Platform: "ubuntu",
			PolicyGroup: "RPGs", PolicyRevision: "fantasy", PolicyName: "boardgames",
			Cookbooks:     []string{"dungeons_n_dragons", "angels", "startwars", "starfinder"},
			Recipes:       []string{"bard", "elf", "human", "barbarian"},
			Roles:         []string{"wizard", "fighter", "necromancer"},
			ResourceNames: []string{"wand", "sword", "bow", "shield"},
			Environment:   "dev", OrganizationName: "org1", ChefVersion: "1.1",
			ChefTags: []string{"chef", "boop"}},
			[]string{"dexterity", "charisma", "strength", "constitution", "intelligence"},
			true,
			"node-"},
		{10, iBackend.NodeInfo{
			Status: "success", Platform: "ubuntu",
			PolicyGroup: "RPGs", PolicyRevision: "fantasy", PolicyName: "boardgames",
			Cookbooks:     []string{"dungeons_n_dragons", "angels", "startwars", "starfinder"},
			Recipes:       []string{"bard", "elf", "human", "barbarian"},
			Roles:         []string{"wizard", "fighter", "necromancer"},
			ResourceNames: []string{"wand", "sword", "bow", "shield"},
			Environment:   "dev", OrganizationName: "org1", ChefVersion: "2.0",
			ChefTags: []string{"cheese"}},
			[]string{"dexterity", "charisma", "strength", "constitution", "intelligence"},
			false,
			"deleted-"},
		{10, iBackend.NodeInfo{
			Status: "failure", Platform: "ubuntu",
			PolicyGroup: "nintendo", PolicyRevision: "zelda", PolicyName: "videogames",
			Cookbooks: []string{"twilight", "ocarina_of_time", "breath_of_the_wild"},
			Recipes:   []string{"zelda", "link", "ganon", "epona"},
			Roles:     []string{"heros", "villains"}, ResourceNames: []string{"triforce"},
			Environment: "prod", OrganizationName: "org3", ChefVersion: "3.0",
			ChefTags: []string{"boop"}},
			[]string{"sword", "shield", "horse", "bow", "finally_can_jump"},
			true,
			"node-"},
		{20, iBackend.NodeInfo{
			Status: "success", Platform: "centos",
			PolicyGroup: "tv_series", PolicyRevision: "friends", PolicyName: "time_to_be_funny",
			Cookbooks:     []string{"rachel", "monica", "chandler", "phoebe", "joey"},
			Recipes:       []string{"season1", "season2", "season3", "you_get_it"},
			ResourceNames: []string{"smart", "joke", "smile", "hilarious", "honest"},
			Environment:   "new-york", OrganizationName: "org1", ChefVersion: "1.1.0",
			ChefTags: []string{"chef"}},
			[]string{"funny", "friendship", "sarcasm"},
			true,
			"node-"},
		{10, iBackend.NodeInfo{
			Status: "failure", Platform: "arch",
			PolicyGroup: "no_tv", PolicyRevision: "no_gum", PolicyName: "no_hats",
			Cookbooks: []string{"breakfast_club"}, Recipes: []string{"students"},
			Roles:       []string{"claire", "john", "allison", "andrew", "brian"},
			Environment: "dev", OrganizationName: "org2", ChefVersion: "2.0",
			ChefTags: []string{"boop"}},
			[]string{"saturday", "detention", "school", "disparate"},
			false,
			"deleted-"},
		{10, iBackend.NodeInfo{
			Status: "failure", Platform: "oracle",
			PolicyGroup: "movies", PolicyRevision: "old_school", PolicyName: "time_to_be_funny",
			Cookbooks: []string{"breakfast_club"}, Recipes: []string{"students"},
			Roles:       []string{"claire", "john", "allison", "andrew", "brian"},
			Environment: "dev", OrganizationName: "org2", ChefVersion: "2.0",
			ChefTags: []string{"boop"}},
			[]string{"saturday", "detention", "school", "disparate"},
			true,
			"node-"},
		{20, iBackend.NodeInfo{
			Status: "missing", Platform: "redhat",
			PolicyGroup: "sports", PolicyRevision: "extream", PolicyName: "games",
			Cookbooks: []string{"ping_pong"}, Recipes: []string{"soccer"},
			Roles: []string{"defence"}, ResourceNames: []string{"attack"},
			Environment: "prod", OrganizationName: "org3", ChefVersion: "2.0",
			ChefTags: []string{"cheese"}},
			[]string{"i_ran_out_of_ideas", "please_forgive_my_typos"},
			true,
			"node-"},
		{20, iBackend.NodeInfo{
			Status: "missing", Platform: "redhat",
			PolicyGroup: "sports", PolicyRevision: "extream", PolicyName: "games",
			Cookbooks: []string{"ping_pong"}, Recipes: []string{"soccer"},
			Roles: []string{"defence"}, ResourceNames: []string{"attack"},
			Environment: "prod", OrganizationName: "org3", ChefVersion: "2.0",
			ChefTags: []string{"cheese"}},
			[]string{"i_ran_out_of_ideas", "please_forgive_my_typos"},
			false,
			"deleted-"},
		{20, iBackend.NodeInfo{
			Status: "missing", Platform: "solaris"},
			[]string{},
			true,
			"node-"},
	}

	var (
		nodes        = make([]iBackend.Node, 0)
		allNodeNames = make([]string, 0)
		index        = 0
	)

	for _, data := range dataNodes {
		for i := 0; i < data.number; i++ {
			data.node.EntityUuid = newUUID()
			data.node.NodeName = data.namePrefix + fmt.Sprintf("%03d", index)
			if data.exists {
				allNodeNames = append(allNodeNames, data.node.NodeName)
			}
			node := iBackend.Node{
				NodeInfo:   data.node,
				Attributes: data.attributes,
				Exists:     data.exists,
			}
			nodes = append(nodes, node)
			index++
		}
	}
	suite.IngestNodes(nodes)
	defer suite.DeleteAllDocuments()

	ctx := context.Background()
	cases := []struct {
		description string
		request     request.Suggestion
		expected    []string
	}{
		// Suggestions for Nodes
		{"should return all nodes suggestions",
			request.Suggestion{Type: "name"},
			allNodeNames},
		{"should return just the set of node names that match",
			request.Suggestion{Type: "name", Text: "node-00"},
			[]string{"node-000", "node-001", "node-002", "node-003", "node-004",
				"node-005", "node-006", "node-007", "node-008", "node-009"}},
		{"should return no matching names",
			request.Suggestion{Type: "name", Text: "deleted-"},
			[]string{}},

		// Suggestions for Environments
		{"should return all environment suggestions",
			request.Suggestion{Type: "environment"},
			[]string{"dev", "prod", "games", "new-york", ""}},
		{"should return zero environment suggestions",
			request.Suggestion{Type: "platform", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'new-york'",
			request.Suggestion{Type: "environment", Text: "n"}, // less than 2 characters we return all results?
			[]string{"dev", "prod", "games", "new-york", ""}},
		{"should return one environment suggestion 'new-york'",
			request.Suggestion{Type: "environment", Text: "ne"},
			[]string{"new-york"}},
		{"should return one environment suggestion 'new-york'",
			request.Suggestion{Type: "environment", Text: "new"},
			[]string{"new-york"}},

		// Suggestions for Platform
		{"should return all platform suggestions",
			request.Suggestion{Type: "platform"},
			[]string{"centos", "redhat", "ubuntu", "oracle", "windows", "solaris"}},
		{"should return zero platform suggestions",
			request.Suggestion{Type: "platform", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'oracle'",
			request.Suggestion{Type: "platform", Text: "o"}, // less than 2 characters we return all results?
			[]string{"centos", "redhat", "ubuntu", "oracle", "windows", "solaris"}},
		{"should return one platform suggestion 'oracle'",
			request.Suggestion{Type: "platform", Text: "or"},
			[]string{"oracle"}},
		{"should return one platform suggestion 'oracle'",
			request.Suggestion{Type: "platform", Text: "ora"},
			[]string{"oracle"}},

		// Suggestions for Policy Group
		{"should return all policy_group suggestions",
			request.Suggestion{Type: "policy_group"},
			[]string{"sports", "tv_series", "rpgs", "heros", "movies", "nintendo", ""}},
		{"should return zero policy_group suggestions",
			request.Suggestion{Type: "policy_group", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'nintendo'",
			request.Suggestion{Type: "policy_group", Text: "n"}, // less than 2 characters we return all results?
			[]string{"sports", "tv_series", "rpgs", "heros", "movies", "nintendo", ""}},
		{"should return one policy_group suggestion 'nintendo'",
			request.Suggestion{Type: "policy_group", Text: "ni"},
			[]string{"nintendo"}},
		{"should return one policy_group suggestion 'nintendo'",
			request.Suggestion{Type: "policy_group", Text: "nin"},
			[]string{"nintendo"}},

		// Suggestions for Policy Name
		{"should return all policy_name suggestions",
			request.Suggestion{Type: "policy_name"},
			[]string{"time_to_be_funny", "games", "boardgames", "comics", "videogames", ""}},
		{"should return zero policy_name suggestions",
			request.Suggestion{Type: "policy_name", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'games'",
			request.Suggestion{Type: "policy_name", Text: "g"}, // less than 2 characters we return all results?
			[]string{"time_to_be_funny", "games", "boardgames", "comics", "videogames", ""}},
		{"should return one policy_name suggestion 'games'",
			request.Suggestion{Type: "policy_name", Text: "ga"},
			[]string{"games"}},
		{"should return one policy_name suggestion 'games'",
			request.Suggestion{Type: "policy_name", Text: "gam"},
			[]string{"games"}},

		// Suggestions for Policy Revision
		{"should return all policy_revision suggestions",
			request.Suggestion{Type: "policy_revision"},
			[]string{"extream", "friends", "fantasy", "marvel", "old_school", "zelda", ""}},
		{"should return zero policy_revision suggestions",
			request.Suggestion{Type: "policy_revision", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'friends'",
			request.Suggestion{Type: "policy_revision", Text: "f"}, // less than 2 characters we return all results?
			[]string{"extream", "friends", "fantasy", "marvel", "old_school", "zelda", ""}},
		{"should return one policy_revision suggestion 'friends'",
			request.Suggestion{Type: "policy_revision", Text: "fr"},
			[]string{"friends"}},
		{"should return one policy_revision suggestion 'friends'",
			request.Suggestion{Type: "policy_revision", Text: "fri"},
			[]string{"friends"}},

		// Suggestions for Recipes
		{"should return all recipe suggestions",
			request.Suggestion{Type: "recipe"},
			[]string{"season1", "season2", "season3", "soccer", "you_get_it", "ant-man",
				"barbarian", "bard", "dr_strange", "elf", "epona", "ganon", "human", "ironman",
				"link", "spiderman", "students", "thor", "zelda"}},
		{"should return zero recipe suggestions",
			request.Suggestion{Type: "recipe", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'season2'",
			request.Suggestion{Type: "recipe", Text: "s"}, // only return the words that has 's'
			[]string{"season1", "season2", "season3", "soccer", "dr_strange", "spiderman", "students"}},
		{"should return one recipe suggestion 'season2'",
			request.Suggestion{Type: "recipe", Text: "se"},
			[]string{"season1", "season3", "season2"}},
		{"should return one recipe suggestion 'season2'",
			request.Suggestion{Type: "recipe", Text: "sea"},
			[]string{"season1", "season3", "season2"}},
		{"should return one recipe suggestion 'season2'",
			request.Suggestion{Type: "recipe", Text: "season2"},
			[]string{"season2"}},

		// Suggestions for Cookbooks
		{"should return all cookbook suggestions",
			request.Suggestion{Type: "cookbook"},
			[]string{"chandler", "joey", "monica", "phoebe", "ping_pong", "rachel", "angels", "avengers",
				"breakfast_club", "breath_of_the_wild", "dungeons_n_dragons", "guardians_of_the_galaxy",
				"justice_league", "ocarina_of_time", "starfinder", "startwars", "twilight"}},
		{"should return zero cookbook suggestions",
			request.Suggestion{Type: "cookbook", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'breakfast_club'",
			request.Suggestion{Type: "cookbook", Text: "b"}, // only return the words that has 'b'
			[]string{"phoebe", "breakfast_club", "breath_of_the_wild"}},
		{"should return one cookbook suggestion 'breakfast_club'",
			request.Suggestion{Type: "cookbook", Text: "br"},
			[]string{"breakfast_club", "breath_of_the_wild"}},
		{"should return one cookbook suggestion 'breakfast_club'",
			request.Suggestion{Type: "cookbook", Text: "bre"},
			[]string{"breakfast_club", "breath_of_the_wild"}},
		{"should return one cookbook suggestion 'breakfast_club'",
			request.Suggestion{Type: "cookbook", Text: "break"},
			[]string{"breakfast_club"}},

		// Suggestions for Resource Names
		{"should return all resource_name suggestions",
			request.Suggestion{Type: "resource_name"},
			[]string{"attack", "hilarious", "honest", "joke", "smart", "smile", "bow", "shield",
				"super_powers", "sword", "triforce", "wand"}},
		{"should return zero resource_name suggestions",
			request.Suggestion{Type: "resource_name", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'super_powers' or 'smart'",
			request.Suggestion{Type: "resource_name", Text: "s"}, // only return the words that has 's'
			[]string{"hilarious", "honest", "smart", "smile", "shield", "super_powers", "sword"}},
		{"should return one resource_name suggestion 'super_powers'",
			request.Suggestion{Type: "resource_name", Text: "su"},
			[]string{"super_powers"}},
		{"should return one resource_name suggestion 'super_powers'",
			request.Suggestion{Type: "resource_name", Text: "sup"},
			[]string{"super_powers"}},
		{"should return one resource_name suggestion 'smart'",
			request.Suggestion{Type: "resource_name", Text: "sm"},
			[]string{"smile", "smart"}},
		{"should return one resource_name suggestion 'smart'",
			request.Suggestion{Type: "resource_name", Text: "sma"},
			[]string{"smart"}},

		// Suggestions for Attributes
		{"should return all attribute suggestions",
			request.Suggestion{Type: "attribute"},
			[]string{"friendship", "funny", "i_ran_out_of_ideas", "please_forgive_my_typos", "sarcasm", "bow",
				"can_fly", "charisma", "complex_attr_lala", "constitution", "detention", "dexterity", "disparate",
				"finally_can_jump", "god", "horse", "immortal", "intelligence", "magic", "saturday", "school",
				"shield", "strength", "sword"}},
		{"should return zero attribute suggestions",
			request.Suggestion{Type: "attribute", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'dexterity'",
			request.Suggestion{Type: "attribute", Text: "d"}, // only return the words that has 'd'
			[]string{"friendship", "i_ran_out_of_ideas", "detention", "dexterity", "disparate",
				"god", "saturday", "shield", "sword"}},
		{"should return one attribute suggestion 'dexterity'",
			request.Suggestion{Type: "attribute", Text: "de"},
			[]string{"dexterity", "detention"}},
		{"should return one attribute suggestion 'dexterity'",
			request.Suggestion{Type: "attribute", Text: "dex"},
			[]string{"dexterity"}},

		// Suggestions for Roles
		{"should return all role suggestions",
			request.Suggestion{Type: "role"},
			[]string{"defence", "heros", "villains", "allison", "andrew", "brian", "claire",
				"fighter", "john", "necromancer", "wizard"}},
		{"should return zero role suggestions",
			request.Suggestion{Type: "role", Text: "lol"},
			[]string{}},
		{"should return results when starting typing 'necromancer'",
			request.Suggestion{Type: "role", Text: "n"}, // only return the words that has 'd'
			[]string{"defence", "villains", "allison", "andrew", "brian", "john", "necromancer"}},
		{"should return one role suggestion 'necromancer'",
			request.Suggestion{Type: "role", Text: "ne"},
			[]string{"necromancer"}},
		{"should return one role suggestion 'necromancer'",
			request.Suggestion{Type: "role", Text: "nec"},
			[]string{"necromancer"}},

		// Suggestions for chef version
		{"should return all chef_version suggestions",
			request.Suggestion{Type: "chef_version"},
			[]string{"1.0", "1.1", "1.1.0", "2.0", "3.0", ""}},
		{"should return zero chef_version suggestions",
			request.Suggestion{Type: "chef_version", Text: "lol"},
			[]string{}},
		{"should return chef_version results when starting typing '1'",
			request.Suggestion{Type: "chef_version", Text: "1."}, // only return the versions that have '1.'
			[]string{"1.0", "1.1", "1.1.0"}},
		{"should return one chef_version suggestion '2.0'",
			request.Suggestion{Type: "chef_version", Text: "2.0"},
			[]string{"2.0"}},

		// Suggestions for chef tags
		{"should return all chef_tags suggestions",
			request.Suggestion{Type: "chef_tags"},
			[]string{"chef", "cheese", "boop"}},
		{"should return zero chef_tags suggestions",
			request.Suggestion{Type: "chef_tags", Text: "lol"},
			[]string{}},
		{"should return chef_tags results when starting typing 'ch'",
			request.Suggestion{Type: "chef_tags", Text: "che"}, // only return the versions that have 'ch.'
			[]string{"chef", "cheese"}},
		{"should return one chef_tags suggestion 'boop'",
			request.Suggestion{Type: "chef_tags", Text: "boop"},
			[]string{"boop"}},
	}

	// Run all the cases!
	for _, test := range cases {
		t.Run(fmt.Sprintf("with request '%v' it %s", test.request, test.description),
			func(t *testing.T) {
				res, err := cfgmgmt.GetSuggestions(ctx, &test.request)
				assert.Nil(t, err)

				// We actually don't care about the scores since it is something
				// the UI uses to order the results, therefore we will just abstract
				// the text into an array and compare it
				actualSuggestionsArray := extractTextFromSuggestionsResponse(res)

				// Verify they both are the same length
				assert.Equal(t, len(test.expected), len(actualSuggestionsArray))

				// Verify that they contains all the fields
				// We don't do 'assert.Equal()' because that checks order
				assert.ElementsMatch(t, actualSuggestionsArray, test.expected)
			})
	}
}

func TestSuggestionsProjectFilter(t *testing.T) {
	cases := []struct {
		description string
		nodes       []iBackend.Node
		ctx         context.Context
		request     request.Suggestion
		expected    []string
	}{
		{
			description: "Two nodes matching on the same project tag",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{"one"}),
			expected: []string{"name1", "name2"},
		},
		{
			description: "Two nodes matching with two project tags",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name2",
					},
					Projects: []string{"one"},
				},
			},
			ctx:      contextWithProjects([]string{"one", "two"}),
			expected: []string{"name1", "name2"},
		},
		{
			description: "Two nodes, one matching",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{"one"}),
			expected: []string{"name2"},
		},
		{
			description: "Matching all",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name1",
					},
					Projects: []string{"three"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name2",
					},
					Projects: []string{"two", "one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name3",
					},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.AllProjectsExternalID}),
			expected: []string{"name1", "name2", "name3"},
		},
		{
			description: "Match one unassigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			expected: []string{"name1"},
		},
		{
			description: "No unassigned; no matches",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name1",
					},
					Projects: []string{"one"},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name2",
					},
					Projects: []string{"two", "one"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID}),
			expected: []string{},
		},
		{
			description: "Match one unassigned and one assigned",
			nodes: []iBackend.Node{
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name1",
					},
					Projects: []string{},
				},
				{
					NodeInfo: iBackend.NodeInfo{
						NodeName: "name2",
					},
					Projects: []string{"two"},
				},
			},
			ctx:      contextWithProjects([]string{authzConstants.UnassignedProjectID, "two"}),
			expected: []string{"name1", "name2"},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("Project filter: %s", test.description), func(t *testing.T) {

			// Adding required node data
			for index := range test.nodes {
				test.nodes[index].Exists = true
				test.nodes[index].NodeInfo.EntityUuid = newUUID()
			}

			// Add node with project
			suite.IngestNodes(test.nodes)
			defer suite.DeleteAllDocuments()

			res, err := cfgmgmt.GetSuggestions(test.ctx, &request.Suggestion{Type: "name"})
			assert.Nil(t, err)

			actualSuggestionsArray := extractTextFromSuggestionsResponse(res)

			assert.ElementsMatch(t, test.expected, actualSuggestionsArray)
		})
	}
}

// extractTextFromSuggestionsResponse will extract only the text from the suggestions
// response:
//
// values {
//   struct_value {
//     fields {
//       key: "score"
//       value {
//         number_value: 1
//       }
//     }
//     fields {
//       key: "text"
//       value {
//         string_value: "dummy"       <--- This are the values we are looking for! :smile:
//       }
//     }
//   }
// }
//
// The only problem with that is that if there are nodes that has empty fields we will find
// something similar to this response:
//
// values {
//   struct_value {
//     fields {
//       key: "score"
//       value {
//         number_value: 1
//       }
//     }
//   }
// }
//
// Where there is NO string value!
//
// TODO: (@afiune) Is this the normal behavior? If not lets fix it.
// for now the fixt in the tests will be to check if there is a "string"
// value or not.
func extractTextFromSuggestionsResponse(list *gp.ListValue) []string {
	// We don't initialize the slice size since we might found empty Values
	textArray := make([]string, 0)

	if list != nil {
		for _, sugg := range list.Values {
			sugStruct := sugg.GetStructValue()
			textArray = append(textArray, sugStruct.Fields["text"].GetStringValue())
		}
	}
	return textArray
}
