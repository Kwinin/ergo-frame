package db

import (
	"github.com/seefan/gossdb"
	"github.com/seefan/gossdb/conf"
)

var pool *gossdb.Connectors

func GetDBClient(Host string, Port int) (*gossdb.Client, error) {
	pool, err := gossdb.NewPool(&conf.Config{
		Host:             Host,
		Port:             Port,
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

func NewDBClient(Host string, Port int) (*DBClient, error) {
	client, err := GetDBClient(Host, Port)
	if err != nil {
		return nil, err
	}

	return &DBClient{
		client: client,
	}, nil
}

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

func (c *DBClient) Del(key string) error {
	return c.client.Del(key)
}

func (c *DBClient) HGet(setName, key string) (string, error) {
	// 获取 Hashmap 类型数据
	resp, err := c.client.Hget(setName, key)
	if err != nil {
		return "", err
	}
	return resp.String(), nil
}

func (c *DBClient) HSet(setName, key string, value string) error {
	return c.client.Hset(setName, key, value)
}

func (c *DBClient) HGetAll(setName string) (map[string]gossdb.Value, error) {
	// 获取 Hashmap 类型数据
	resp, err := c.client.HgetAll(setName)
	if err != nil {
		return nil, err
	}
	return resp, nil
}

func (c *DBClient) MultiHSet(setName string, kvs map[string]interface{}) error {
	return c.client.MultiHset(setName, kvs)
}
