package db

import (
	"github.com/seefan/gossdb"
	"github.com/seefan/gossdb/conf"
	"wsgate/config"
)

var pool *gossdb.Connectors

func GetDBClient() (*gossdb.Client, error) {
	pool, err := gossdb.NewPool(&conf.Config{
		Host:             config.Cfg.SSDB.Host,
		Port:             config.Cfg.SSDB.Port,
		MinPoolSize:      5,
		MaxPoolSize:      50,
		AcquireIncrement: 5,
	})
	if err != nil {
		return nil, err
	}
	return pool.NewClient()
}

type DBClient struct {
	client *gossdb.Client
}

//type Sdb struct {
//}
//
//func (d Sdb) NewDBClient() (*DBClient, error) {
//	client, err := GetDBClient()
//	if err != nil {
//		return nil, err
//	}
//
//	return &DBClient{
//		client: client,
//	}, nil
//}

func (c *DBClient) Close() {
	c.client.Close()
}

func (c *DBClient) Get(key string) (string, error) {
	resp, err := c.client.Get(key)
	if err != nil {
		return "", err
	}

	return resp.String(), nil
}

func (c *DBClient) Set(key string, value string) error {
	return c.client.Set(key, []byte(value))
}
