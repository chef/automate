package v2_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	v2 "github.com/chef/automate/api/interservice/teams/v2"
)

func TestGetTeamReq(t *testing.T) {
	negativeCases := map[string]*v2.GetTeamReq{
		"empty ID": &v2.GetTeamReq{
			Id: "",
		},
		"whitespace ID": &v2.GetTeamReq{
			Id: "      ",
		},
		"missing ID": &v2.GetTeamReq{},
	}
	positiveCases := map[string]*v2.GetTeamReq{
		"with spaces until GA": &v2.GetTeamReq{
			Id: "1111valid ID for now~~~",
		},
		"alphanumeric-post-GA": &v2.GetTeamReq{
			Id: "asdf-123-fun",
		},
	}

	classes := map[bool]map[string]*v2.GetTeamReq{
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
	negativeCases := map[string]*v2.DeleteTeamReq{
		"empty ID": &v2.DeleteTeamReq{
			Id: "",
		},
		"whitespace ID": &v2.DeleteTeamReq{
			Id: "       ",
		},
		"missing ID": &v2.DeleteTeamReq{},
	}
	positiveCases := map[string]*v2.DeleteTeamReq{
		"with spaces until GA": &v2.DeleteTeamReq{
			Id: "1111valid ID for now~~~",
		},
		"alphanumeric-post-GA": &v2.DeleteTeamReq{
			Id: "asdf-123-fun",
		},
	}

	classes := map[bool]map[string]*v2.DeleteTeamReq{
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
	negativeCases := map[string]*v2.UpdateTeamReq{
		"empty ID": &v2.UpdateTeamReq{
			Id:       "",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"whitespace ID": &v2.UpdateTeamReq{
			Id:       "      ",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"missing ID": &v2.UpdateTeamReq{
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"empty Name": &v2.UpdateTeamReq{
			Id:       "test",
			Name:     "",
			Projects: []string{"test"},
		},
		"whitespace Name": &v2.UpdateTeamReq{
			Id:       "test",
			Name:     "          ",
			Projects: []string{"test"},
		},
		"missing Name": &v2.UpdateTeamReq{
			Id:       "test",
			Projects: []string{"test"},
		},
		"missing Name, ID, and Projects": &v2.UpdateTeamReq{},
		"whitespace projects list": &v2.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"     ", "test"},
		},
		"repeated projects in list": &v2.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"repeat", "repeat"},
		},
		"Project has invalid characters": &v2.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong~"},
		},
		"Project has spaces": &v2.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong space"},
		},
	}

	positiveCases := map[string]*v2.UpdateTeamReq{
		"with spaces until GA": &v2.UpdateTeamReq{
			Id:       "1111valid ID for now~~~",
			Name:     "this is valid ~ fun characters",
			Projects: []string{"test", "test-2"},
		},
		"alphanumeric-post-GA": &v2.UpdateTeamReq{
			Id:       "asdf-123-fun",
			Name:     "this is valid ~ fun characters",
			Projects: []string{"test", "test-2"},
		},
		"missing projects list": &v2.UpdateTeamReq{
			Id:   "this-is-valid-1",
			Name: "name of my team ~ fun characters 1 %",
		},
		"empty projects list": &v2.UpdateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{},
		},
	}

	classes := map[bool]map[string]*v2.UpdateTeamReq{
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
	negativeCases := map[string]*v2.CreateTeamReq{
		"empty ID": &v2.CreateTeamReq{
			Id:       "",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"whitespace ID": &v2.CreateTeamReq{
			Id:       "           ",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"missing ID": &v2.CreateTeamReq{
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"empty Name": &v2.CreateTeamReq{
			Id:       "test",
			Name:     "",
			Projects: []string{"test"},
		},
		"whitespace Name": &v2.CreateTeamReq{
			Id:       "test",
			Name:     "          ",
			Projects: []string{"test"},
		},
		"missing Name": &v2.CreateTeamReq{
			Id:       "test",
			Projects: []string{"test"},
		},
		"missing Name, ID, and projects": &v2.CreateTeamReq{},
		"ID has invalid characters": &v2.CreateTeamReq{
			Id:       "wrong~",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"ID has spaces": &v2.CreateTeamReq{
			Id:       "wrong space",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"ID has valid characters but is too long": &v2.CreateTeamReq{
			Id:       "super-duper-long-1232-pneumonoultramicroscopicsilicovolcanoconiosis",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"whitespace projects list": &v2.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"     ", "test"},
		},
		"repeated projects in list": &v2.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"repeat", "repeat"},
		},
		"Project has invalid characters": &v2.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong~"},
		},
		"Project has spaces": &v2.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong space"},
		},
	}

	positiveCases := map[string]*v2.CreateTeamReq{
		"ID present with a name": &v2.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"test", "test-2"},
		},
		"empty projects list": &v2.CreateTeamReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{},
		},
		"missing projects list": &v2.CreateTeamReq{
			Id:   "this-is-valid-1",
			Name: "name of my team ~ fun characters 1 %",
		},
	}

	classes := map[bool]map[string]*v2.CreateTeamReq{
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
	negativeCases := map[string]*v2.AddTeamMembersReq{
		"empty ID": &v2.AddTeamMembersReq{
			Id:      "",
			UserIds: []string{"valid"},
		},
		"whitespace ID": &v2.AddTeamMembersReq{
			Id:      "    ",
			UserIds: []string{"valid"},
		},
		"missing ID": &v2.AddTeamMembersReq{
			UserIds: []string{"valid"},
		},
		"empty user list": &v2.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{},
		},
		"missing user list": &v2.AddTeamMembersReq{
			Id: "valid",
		},
		"whitespace user list": &v2.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"     ", "test"},
		},
		"repeated users in list": &v2.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"repeat", "repeat"},
		},
		"User ID has invalid characters": &v2.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"valid", "wrong~"},
		},
		"User ID has spaces": &v2.AddTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"valid", "wrong space"},
		},
	}

	positiveCases := map[string]*v2.AddTeamMembersReq{
		"single user ID present with crazy ID until GA": &v2.AddTeamMembersReq{
			Id:      "1111valid ID for now~~~",
			UserIds: []string{"this-is-valid-1@gmail+chef.com"},
		},
		"multiple unique User IDs present with crazy ID until GA (including a GUID)": &v2.AddTeamMembersReq{
			Id:      "1111valid ID for now~~~",
			UserIds: []string{"this-is-valid-1", "this-is-valid-2@gmail+chef.com", "6413bd61-d532-47d2-b842-0a2c9f3a8ce1"},
		},
	}

	classes := map[bool]map[string]*v2.AddTeamMembersReq{
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
	negativeCases := map[string]*v2.RemoveTeamMembersReq{
		"empty ID": &v2.RemoveTeamMembersReq{
			Id:      "",
			UserIds: []string{"valid"},
		},
		"whitespace ID": &v2.RemoveTeamMembersReq{
			Id:      "      ",
			UserIds: []string{"valid"},
		},
		"missing ID": &v2.RemoveTeamMembersReq{
			UserIds: []string{"valid"},
		},
		"empty user list": &v2.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{},
		},
		"missing user list": &v2.RemoveTeamMembersReq{
			Id: "valid",
		},
		"whitespace user list": &v2.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"   ", "test"},
		},
		"repeated users in list": &v2.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"repeat", "repeat"},
		},
		"User ID has invalid characters": &v2.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"valid", "wrong~"},
		},
		"User ID has spaces": &v2.RemoveTeamMembersReq{
			Id:      "valid",
			UserIds: []string{"valid", "wrong space"},
		},
	}

	positiveCases := map[string]*v2.RemoveTeamMembersReq{
		"single user ID present with crazy ID until GA": &v2.RemoveTeamMembersReq{
			Id:      "1111valid ID for now~~~",
			UserIds: []string{"this-is-valid-1@gmail+chef.com"},
		},
		"multiple unique User IDs present with crazy ID until GA (including a GUID)": &v2.RemoveTeamMembersReq{
			Id:      "1111valid ID for now~~~",
			UserIds: []string{"this-is-valid-1", "this-is-valid-2@gmail+chef.com", "6413bd61-d532-47d2-b842-0a2c9f3a8ce1"},
		},
	}

	classes := map[bool]map[string]*v2.RemoveTeamMembersReq{
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
	negativeCases := map[string]*v2.GetTeamsForMemberReq{
		"empty ID": &v2.GetTeamsForMemberReq{
			UserId: "",
		},
		"whitespace ID": &v2.GetTeamsForMemberReq{
			UserId: "        ",
		},
		"missing ID": &v2.GetTeamsForMemberReq{},
		"ID has invalid characters": &v2.GetTeamsForMemberReq{
			UserId: "wrong~",
		},
		"ID has spaces": &v2.GetTeamsForMemberReq{
			UserId: "wrong space",
		},
	}

	positiveCases := map[string]*v2.GetTeamsForMemberReq{
		"when the ID is an email": &v2.GetTeamsForMemberReq{
			UserId: "this-is-valid-1@gmail+chef.com",
		},
		"when the ID is a GUID": &v2.GetTeamsForMemberReq{
			UserId: "6413bd61-d532-47d2-b842-0a2c9f3a8ce1",
		},
	}

	classes := map[bool]map[string]*v2.GetTeamsForMemberReq{
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
	negativeCases := map[string]*v2.GetTeamMembershipReq{
		"empty ID": &v2.GetTeamMembershipReq{
			Id: "",
		},
		"whitespace ID": &v2.GetTeamMembershipReq{
			Id: "     ",
		},
		"missing ID": &v2.GetTeamMembershipReq{},
	}
	positiveCases := map[string]*v2.GetTeamMembershipReq{
		"with spaces until GA": &v2.GetTeamMembershipReq{
			Id: "1111valid ID for now~~~",
		},
		"alphanumeric-post-GA": &v2.GetTeamMembershipReq{
			Id: "asdf-123-fun",
		},
	}

	classes := map[bool]map[string]*v2.GetTeamMembershipReq{
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
