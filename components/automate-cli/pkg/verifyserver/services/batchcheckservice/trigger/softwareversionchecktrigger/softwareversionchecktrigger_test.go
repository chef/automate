package softwareversionchecktrigger

// func TestNewSoftwareVersionCheck_Run(t *testing.T) {
// 	t.Run("Test 1. empty val", func(t *testing.T) {
// 		ss := NewSoftwareVersionCheck()
// 		config := models.Config{}
// 		result := ss.Run(config)
// 		require.Empty(t, result)
// 	})

// 	t.Run("Test 2. models.CheckTriggerResponse map", func(t *testing.T) {
// 		ss := NewSoftwareVersionCheck()
// 		config := models.Config{
// 			Hardware: models.Hardware{
// 				AutomateNodeIps:   []string{"127.0.0.1", "localhost"},
// 				AutomateNodeCount: 2,
// 			},
// 		}
// 		result := ss.Run(config)
// 		require.NotEmpty(t, result)
// 		require.NotEmpty(t, result["127.0.0.1"])
// 		require.NotEmpty(t, result["localhost"])
// 	})

// 	t.Run("Test 3. not-reachable-api", func(t *testing.T) {
// 		ss := NewSoftwareVersionCheck()
// 		config := models.Config{
// 			Hardware: models.Hardware{
// 				AutomateNodeIps:   []string{"not-reachable-api"},
// 				AutomateNodeCount: 1,
// 			},
// 		}
// 		result := ss.Run(config)
// 		fmt.Printf("result: %v\n", result)
// 		require.NotEmpty(t, result)
// 		require.NotEmpty(t, result["not-reachable-api"])
// 		require.Equal(t, result["not-reachable-api"].Error, `error triggering the API https://not-reachable-api/api/v1/checks/software-versions?node_type=automate: Get "https://not-reachable-api/api/v1/checks/software-versions?node_type=automate": dial tcp: lookup not-reachable-api: no such host`)
// 	})
// }
