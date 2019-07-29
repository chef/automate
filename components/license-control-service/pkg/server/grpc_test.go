package server_test

// func TestHealthCheck(t *testing.T) {
// 	ctx, cancel := context.WithCancel(context.Background())
// 	defer cancel()

// 	viper.SetConfigFile("./testdata/config.toml")

// 	err := viper.ReadInConfig()
// 	require.NoError(t, err, "reading config file")

// 	cfg, err := server.ConfigFromViper()
// 	require.NoError(t, err, "configuring service")

// 	srv, err := server.NewGRPC(ctx, cfg)
// 	require.NoError(t, err, "initializing grpc server")

// 	connFactory := secureconn.NewFactory(*cfg.ServiceCerts)
// 	conn, err := connFactory.Dial("license-control-service", cfg.ListenAddress())
// 	require.NoError(t, err, "dialing license-control-service")

// 	cl := healthpb.NewHealthClient(conn)
// 	t.Run("Check", func(t *testing.T) {
// 		actual, err := cl.Check(ctx, &healthpb.HealthCheckRequest{})
// 		require.NoError(t, err)
// 		assert.Equal(t, healthpb.HealthCheckResponse_SERVING, actual.GetStatus())
// 	})
// }
