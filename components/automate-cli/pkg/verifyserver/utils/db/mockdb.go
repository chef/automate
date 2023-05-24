package db

type MockDB struct {
    InitPostgresDBFunc func(con string) error
}

func (m *MockDB) InitPostgresDB(con string) error {
    return m.InitPostgresDBFunc(con)
}
