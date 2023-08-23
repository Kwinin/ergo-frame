package test

import (
	"encoding/json"
	"fmt"
	"master/db"

	"testing"
)

func TestFixJson(t *testing.T) {

	client, err := db.GetDBClient("127.0.0.1", 8888)
	if err != nil {
		t.Fatalf("无法连接到SSDB数据库：%v", err)
	}
	defer client.Close()

	type UserData struct {
		Name     string `json:"name"`
		Age      int    `json:"age"`
		Email    string `json:"email"`
		IsMember bool   `json:"is_member"`
	}

	users := []UserData{
		{
			Name:     "Alice",
			Age:      25,
			Email:    "alice@example.com",
			IsMember: true,
		},
		{
			Name:     "Bob",
			Age:      30,
			Email:    "bob@example.com",
			IsMember: false,
		},
	}

	// 新的 UserData
	newUser := UserData{
		Name:     "Charlie",
		Age:      28,
		Email:    "charlie@example.com",
		IsMember: true,
	}

	// 使用 append 函数追加新的值
	users = append(users, newUser)

	// 1. JSON 序列化
	jsonData, err := json.Marshal(users)
	if err != nil {
		fmt.Println("JSON serialization error:", err)
		return
	}

	keyToStore := "user_data"
	err = client.Set(keyToStore, string(jsonData))
	if err != nil {
		fmt.Println("SSDB set error:", err)
		return
	}

	// 3. 反序列化
	storedJSONData, err := client.Get(keyToStore)
	if err != nil {
		fmt.Println("SSDB get error:", err)
		return
	}

	var storedData []UserData
	err = json.Unmarshal([]byte(storedJSONData), &storedData)
	if err != nil {
		fmt.Println("JSON deserialization error:", err)
		return
	}

	// 现在 storedData 就是原始的复杂数据类型（UserData 结构体）了
	fmt.Printf("111,%+v\n", storedData)
}
