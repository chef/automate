package teams_test

import (
	"testing"

	"github.com/chef/automate/api/interservice/teams"
	"github.com/stretchr/testify/assert"
)

func TestGetTeamReq(t *testing.T) {
	negativeCases := map[string]*teams.GetTeamReq{
		"empty ID": &teams.GetTeamReq{
			Id: "",
		},
		"whitespace ID": &teams.GetTeamReq{
			Id: "      ",
		},
		"missing ID": &teams.GetTeamReq{},
	}
	positiveCases := map[string]*teams.GetTeamReq{
		"with spaces until GA": &teams.GetTeamReq{
			Id: "1111valid ID for now~~~",
		},
		"alphanumeric-post-GA": &teams.GetTeamReq{
			Id: "asdf-123-fun",
		},
	}

	classes := map[bool]map[string]*teams.GetTeamReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}

func TestDeleteTeamReq(t *testing.T) {
	negativeCases := map[string]*teams.DeleteTeamReq{
		"empty ID": &teams.DeleteTeamReq{
			Id: "",
		},
		"whitespace ID": &teams.DeleteTeamReq{
			Id: "       ",
		},
		"missing ID": &teams.DeleteTeamReq{},
	}
	positiveCases := map[string]*teams.DeleteTeamReq{
		"with spaces until GA": &teams.DeleteTeamReq{
			Id: "1111valid ID for now~~~",
		},
		"alphanumeric-post-GA": &teams.DeleteTeamReq{
			Id: "asdf-123-fun",
		},
	}

	classes := map[bool]map[string]*teams.DeleteTeamReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}

func TestUpdateTeamReq(t *testing.T) {
	negativeCases := map[string]*teams.UpdateTeamReq{
		"empty ID": &teams.UpdateTeamReq{
			Id:       "",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"whitespace ID": &teams.UpdateTeamReq{
			Id:       "      ",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"missing ID": &teams.UpdateTeamReq{
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"missing Name, ID, and Projects": &teams.UpdateTeamReq{},
		"whitespace projects list": &teams.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"     ", "test"},
		},
		"repeated projects in list": &teams.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"repeat", "repeat"},
		},
		"Project has invalid characters": &teams.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong~"},
		},
		"Project has spaces": &teams.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong space"},
		},
	}

	positiveCases := map[string]*teams.UpdateTeamReq{
		"with spaces until GA": &teams.UpdateTeamReq{
			Id:       "1111valid ID for now~~~",
			Name:     "this is valid ~ fun characters",
			Projects: []string{"test", "test-2"},
		},
		"alphanumeric-post-GA": &teams.UpdateTeamReq{
			Id:       "asdf-123-fun",
			Name:     "this is valid ~ fun characters",
			Projects: []string{"test", "test-2"},
		},
		"missing projects list": &teams.UpdateTeamReq{
			Id:   "this-is-valid-1",
			Name: "name of my team ~ fun characters 1 %",
		},
		"empty projects list": &teams.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{},
		},
	}

	classes := map[bool]map[string]*teams.UpdateTeamReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}

func TestCreateTeamReq(t *testing.T) {
	negativeCases := map[string]*teams.CreateTeamReq{
		"empty ID": &teams.CreateTeamReq{
			Id:       "",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"whitespace ID": &teams.CreateTeamReq{
			Id:       "           ",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"missing ID": &teams.CreateTeamReq{
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"missing Name, ID, and projects": &teams.CreateTeamReq{},
		"ID has invalid characters": &teams.CreateTeamReq{
			Id:       "wrong~",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"ID has spaces": &teams.CreateTeamReq{
			Id:       "wrong space",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"ID has valid characters but is too long": &teams.CreateTeamReq{
			Id:       "super-duper-long-1232-pneumonoultramicroscopicsilicovolcanoconiosis",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"whitespace projects list": &teams.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"     ", "test"},
		},
		"repeated projects in list": &teams.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"repeat", "repeat"},
		},
		"Project has invalid characters": &teams.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong~"},
		},
		"Project has spaces": &teams.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong space"},
		},
	}

	positiveCases := map[string]*teams.CreateTeamReq{
		"ID present with a name": &teams.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"test", "test-2"},
		},
		"empty projects list": &teams.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{},
		},
		"missing projects list": &teams.CreateTeamReq{
			Id:   "this-is-valid-1",
			Name: "name of my team ~ fun characters 1 %",
		},
	}

	classes := map[bool]map[string]*teams.CreateTeamReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}

func TestAddTeamMembersReq(t *testing.T) {
	negativeCases := map[string]*teams.AddTeamMembersReq{
		"empty ID": &teams.AddTeamMembersReq{
			Id:      "",
			UserIds: []string{"valid"},
		},
		"whitespace ID": &teams.AddTeamMembersReq{
			Id:      "    ",
			UserIds: []string{"valid"},
		},
		"missing ID": &teams.AddTeamMembersReq{
			UserIds: []string{"valid"},
		},
		"empty user list": &teams.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{},
		},
		"missing user list": &teams.AddTeamMembersReq{
			Id: "valid",
		},
		"whitespace user list": &teams.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"     ", "test"},
		},
		"repeated users in list": &teams.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"repeat", "repeat"},
		},
		"User ID has invalid characters": &teams.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"valid", "wrong~"},
		},
		"User ID has spaces": &teams.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"valid", "wrong space"},
		},
	}

	positiveCases := map[string]*teams.AddTeamMembersReq{
		"single user ID present with crazy ID until GA": &teams.AddTeamMembersReq{
			Id:      "1111valid ID for now~~~",
			UserIds: []string{"this-is-valid-1@gmail+chef.com"},
		},
		"multiple unique User IDs present with crazy ID until GA (including a GUID)": &teams.AddTeamMembersReq{
			Id:      "1111valid ID for now~~~",
			UserIds: []string{"this-is-valid-1", "this-is-valid-2@gmail+chef.com", "6413bd61-d532-47d2-b842-0a2c9f3a8ce1"},
		},
	}

	classes := map[bool]map[string]*teams.AddTeamMembersReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}

func TestRemoveTeamMembersReq(t *testing.T) {
	negativeCases := map[string]*teams.RemoveTeamMembersReq{
		"empty ID": &teams.RemoveTeamMembersReq{
			Id:      "",
			UserIds: []string{"valid"},
		},
		"whitespace ID": &teams.RemoveTeamMembersReq{
			Id:      "      ",
			UserIds: []string{"valid"},
		},
		"missing ID": &teams.RemoveTeamMembersReq{
			UserIds: []string{"valid"},
		},
		"empty user list": &teams.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{},
		},
		"missing user list": &teams.RemoveTeamMembersReq{
			Id: "valid",
		},
		"whitespace user list": &teams.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"   ", "test"},
		},
		"repeated users in list": &teams.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"repeat", "repeat"},
		},
		"User ID has invalid characters": &teams.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"valid", "wrong~"},
		},
		"User ID has spaces": &teams.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"valid", "wrong space"},
		},
	}

	positiveCases := map[string]*teams.RemoveTeamMembersReq{
		"single user ID present with crazy ID until GA": &teams.RemoveTeamMembersReq{
			Id:      "1111valid ID for now~~~",
			UserIds: []string{"this-is-valid-1@gmail+chef.com"},
		},
		"multiple unique User IDs present with crazy ID until GA (including a GUID)": &teams.RemoveTeamMembersReq{
			Id:      "1111valid ID for now~~~",
			UserIds: []string{"this-is-valid-1", "this-is-valid-2@gmail+chef.com", "6413bd61-d532-47d2-b842-0a2c9f3a8ce1"},
		},
	}

	classes := map[bool]map[string]*teams.RemoveTeamMembersReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}

func TestGetTeamsForMemberReq(t *testing.T) {
	negativeCases := map[string]*teams.GetTeamsForMemberReq{
		"empty ID": &teams.GetTeamsForMemberReq{
			UserId: "",
		},
		"whitespace ID": &teams.GetTeamsForMemberReq{
			UserId: "        ",
		},
		"missing ID": &teams.GetTeamsForMemberReq{},
		"ID has invalid characters": &teams.GetTeamsForMemberReq{
			UserId: "wrong~",
		},
		"ID has spaces": &teams.GetTeamsForMemberReq{
			UserId: "wrong space",
		},
	}

	positiveCases := map[string]*teams.GetTeamsForMemberReq{
		"when the ID is an email": &teams.GetTeamsForMemberReq{
			UserId: "this-is-valid-1@gmail+chef.com",
		},
		"when the ID is a GUID": &teams.GetTeamsForMemberReq{
			UserId: "6413bd61-d532-47d2-b842-0a2c9f3a8ce1",
		},
	}

	classes := map[bool]map[string]*teams.GetTeamsForMemberReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}

func TestGetTeamMembershipReq(t *testing.T) {
	negativeCases := map[string]*teams.GetTeamMembershipReq{
		"empty ID": &teams.GetTeamMembershipReq{
			Id: "",
		},
		"whitespace ID": &teams.GetTeamMembershipReq{
			Id: "     ",
		},
		"missing ID": &teams.GetTeamMembershipReq{},
	}
	positiveCases := map[string]*teams.GetTeamMembershipReq{
		"with spaces until GA": &teams.GetTeamMembershipReq{
			Id: "1111valid ID for now~~~",
		},
		"alphanumeric-post-GA": &teams.GetTeamMembershipReq{
			Id: "asdf-123-fun",
		},
	}

	classes := map[bool]map[string]*teams.GetTeamMembershipReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}
