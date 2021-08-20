package server

import (
	"testing"
)

func TestVerboseUserslist(t *testing.T) {
	setup()
	defer teardown()

	mux.HandleFunc("/users", func(w http.ResponseWriter, r *http.Request) {
		switch {
		case r.Method == "GET":
			fmt.Fprintf(w, `{
                                "janechef": { "email": "jane.chef@user.com", "first_name": "jane", "last_name": "chef_user" },
                                "yaelsmith": { "email": "yael.chef@user.com", "first_name": "yael", "last_name": "smith" }
                        }`)

		}
	})

	// Test list
	users, err := client.Users.VerboseList()
	if err != nil {
		t.Errorf("Verbose Users.List returned error: %v", err)
	}
	jane := UserVerboseResult{Email: "jane.chef@user.com", FirstName: "jane", LastName: "chef_user"}
	yael := UserVerboseResult{Email: "yael.chef@user.com", FirstName: "yael", LastName: "smith"}
	listWant := map[string]UserVerboseResult{"janechef": jane, "yaelsmith": yael}
	if !reflect.DeepEqual(users, listWant) {
		t.Errorf("Verbose Users.List returned %+v, want %+v", users, listWant)
	}
}
