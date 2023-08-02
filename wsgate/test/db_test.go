package test

import (
	"encoding/json"
	"fmt"
	"github.com/fatih/structs"
	"github.com/mitchellh/mapstructure"
	"testing"
	"wsgate/apps/wsgateapp/db"
	"wsgate/util"
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

	// 设置 Hashmap 类型数据
	err = pool.Hset("user:1", "name", "Alice")
	if err != nil {
		fmt.Println("Failed to set Hashmap:", err)
		return
	}

	// 获取 Hashmap 类型数据
	name, err := pool.Hget("user:1", "name")
	if err != nil {
		fmt.Println("Failed to get Hashmap:", err)
		return
	}

	fmt.Println("Name of user 1:", name)

	fmt.Println(111, result)
}

func TestDbPoolDel(t *testing.T) {
	pool, err := db.GetDBClient("127.0.0.1", 8888)
	if err != nil {
		fmt.Println("Failed to create SSDB connection pool:", err)
		return
	}
	defer pool.Close()
	err = pool.Del("my_key")
	err = pool.Del("openid:openid_34")
	if err != nil {
		fmt.Println("Failed to set key:", err)
		return
	}
}

// 定义一个示例的数据结构
type Person struct {
	ID   int
	Name string
	Age  int
}

func TestLoadAndSaveStructData(t *testing.T) {
	// 创建一个新的DBClient
	client, err := db.NewDBClient("127.0.0.1", 8888)
	if err != nil {
		t.Fatalf("无法连接到SSDB数据库：%v", err)
	}
	defer client.Close()

	// 创建一个结构体实例
	person := Person{
		ID:   434,
		Name: "kwinin11",
		Age:  10,
	}

	// 将结构体转换成 map
	personData := structs.Map(person)
	fmt.Println(111, personData)

	// 设置 Hashmap 类型数据
	err = client.MultiHSet("person:1", personData)
	if err != nil {
		fmt.Println("Failed to set Hashmap:", err)
		return
	}

}

func TestMapMapToStruct(t *testing.T) {

	//type Person struct {
	//	Name string
	//	Age  int
	//}

	client, err := db.NewDBClient("127.0.0.1", 8888)
	if err != nil {
		t.Fatalf("无法连接到SSDB数据库：%v", err)
	}
	defer client.Close()

	//personData := map[string]gossdb.Value{
	//	"Name": "John",
	//	"Age":  "50",
	//}
	personData, err := client.HGetAll("person:1")
	if err != nil {
		fmt.Println("Failed to get Hashmap:", err)
		return
	}
	fmt.Println(3333, personData)

	// 使用通用的函数将map[string]gossdb.Value转换为struct
	person, err := util.MapToStruct(personData, &Person{})
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	fmt.Println("Name:", person.(*Person).Name)
	fmt.Println("Age:", person.(*Person).Age)

	// 再次进行类型断言，获取原始的结构体类型
	p, ok := person.(*Person)
	if !ok {
		fmt.Println("Error: type assertion failed")
		return
	}

	// 现在可以直接使用 p 来访问结构体的字段
	fmt.Println("Name1:", p.Name)
	fmt.Println("Age1:", p.Age)
}

func TestMap(t *testing.T) {

	type Person struct {
		Name string
		Age  int
	}

	personData := map[string]interface{}{
		"Name": "John",
		"Age":  30,
	}

	var person Person
	err := mapstructure.Decode(personData, &person)
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	fmt.Println("Name:", person.Name)
	fmt.Println("Age:", person.Age)

}

func TestJson(t *testing.T) {
	client, err := db.NewDBClient("127.0.0.1", 8888)
	if err != nil {
		t.Fatalf("无法连接到SSDB数据库：%v", err)
	}
	defer client.Close()

	type Person struct {
		Name   string
		Age    int
		Scores []int
	}

	// 假设你有一个名为 client 的 gossdb 客户端实例

	person := Person{
		Name:   "John",
		Age:    30,
		Scores: []int{90, 80, 95},
	}

	// 将 Scores 数组转换为 JSON 字符串
	scoresJSON, err := json.Marshal(person.Scores)
	if err != nil {
		fmt.Println("Error encoding Scores:", err)
		return
	}

	// 使用 MultiHset 存储数据
	err = client.MultiHSet("person:1", map[string]interface{}{
		"Name":   person.Name,
		"Age":    person.Age,
		"Scores": string(scoresJSON), // 将 JSON 字符串存储为字符串类型的值
	})

	if err != nil {
		fmt.Println("Error storing data:", err)
		return
	}

	// 从 SSDB 中获取数据
	data, err := client.HGetAll("person:1")
	if err != nil {
		fmt.Println("Error getting data:", err)
		return
	}

	// 将 JSON 字符串解码为数组
	var storedPerson Person
	storedPerson.Name = data["Name"].String()
	storedPerson.Age = data["Age"].Int()

	var storedScores []int
	err = json.Unmarshal([]byte(data["Scores"].String()), &storedScores)
	if err != nil {
		fmt.Println("Error decoding Scores:", err)
		return
	}
	storedPerson.Scores = storedScores
	fmt.Printf("34343, %+v", storedPerson)

}

func TestFixJson(t *testing.T) {

	client, err := db.NewDBClient("127.0.0.1", 8888)
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

	// 假设有一个复杂数据类型（UserData 结构体）需要存储
	dataToStore := UserData{
		Name:     "John Doe",
		Age:      30,
		Email:    "john@example.com",
		IsMember: true,
	}

	// 1. JSON 序列化
	jsonData, err := json.Marshal(dataToStore)
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

	var storedData UserData
	err = json.Unmarshal([]byte(storedJSONData), &storedData)
	if err != nil {
		fmt.Println("JSON deserialization error:", err)
		return
	}

	// 现在 storedData 就是原始的复杂数据类型（UserData 结构体）了
	fmt.Printf("%+v\n", storedData)
}

func TestIds(t *testing.T) {
	numSegments := 4

	userIDs := []int{123, 456, 789, 101, 202, 303}

	for _, userID := range userIDs {
		segment := userID % numSegments
		fmt.Printf("User %d is in segment %d\n", userID, segment)
	}
}
