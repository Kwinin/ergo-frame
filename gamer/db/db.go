package db

type InfDb interface {
	NewDBClient() (*DBClient, error)
}
