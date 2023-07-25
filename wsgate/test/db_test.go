package test

import (
	"fmt"
	"testing"
	"wsgate/apps/wsgateapp/db"
)

func TestDbPool(t *testing.T) {
	// 初始化连接池
	pool, err := db.GetDBClient("127.0.0.1", 8888)
	if err != nil {
		fmt.Println("Failed to create SSDB connection pool:", err)
		return
	}
	defer pool.Close()

	key := "my_key"
	value := "kwinin_value"
	err = pool.Set(key, value)
	if err != nil {
		fmt.Println("Failed to set key:", err)
		return
	}
	result, rerr := pool.Get(key)
	if rerr != nil {
		fmt.Println("Failed to set key:", rerr)
		return
	}
	fmt.Println(111, result)
}
