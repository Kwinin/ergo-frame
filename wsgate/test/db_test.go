package test

import (
	"fmt"
	"testing"
	"wsgate/apps/db"
)

func TestDbPool(t *testing.T) {
	// 初始化连接池
	pool, err := db.NewSSDBConnectionPool("127.0.0.1", 8888, 10)
	if err != nil {
		fmt.Println("Failed to create SSDB connection pool:", err)
		return
	}
	defer pool.Close()

	// 在需要使用数据库连接的地方，从连接池中获取连接
	client, err := pool.Get()
	if err != nil {
		fmt.Println("Failed to get connection from pool:", err)
		return
	}
	defer pool.Put(client) // 确保在使用完连接后将其归还给连接池

	// 使用连接执行数据库操作
	key := "my_key"
	value := "my_value"
	_, err = client.Set(key, value)
	if err != nil {
		fmt.Println("Failed to set key:", err)
		return
	}
	// 其他数据库操作...
}
