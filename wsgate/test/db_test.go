package test

import (
	"encoding/json"
	"fmt"
	"testing"
	"wsgate/db"
	"wsgate/log"
)

func TestDBCon(t *testing.T) {
	db, err := db.InfDb.NewDBClient(&db.SctSSdb{})
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}

	type Item struct {
		Code int32
		Name string
	}
	type ShopModel struct {
		Items    []Item
		PlayerId int32
	}

	df := ShopModel{
		Items: []Item{
			{Code: 1, Name: "Item 1"},
			{Code: 2, Name: "Item 24"},
		},
		PlayerId: 34444,
	}

	// 将 Items 切片转化为JSON字符串
	itemsJSON, err := json.Marshal(df.Items)
	if err != nil {
		// 处理错误
		fmt.Println("JSON编码错误：", err)
		return
	}

	// 创建一个新的 map，其中 Items 字段存储为 JSON 字符串
	data := map[string]interface{}{
		"Items":    string(itemsJSON),
		"PlayerId": df.PlayerId,
	}

	// 使用 map 存储数据
	err = db.MultiHSet("test_shop", data)
	if err != nil {
		fmt.Println("Hmset 错误：", err)
		return
	}
}

func TestDBGet(t *testing.T) {
	db, err := db.InfDb.NewDBClient(&db.SctSSdb{})
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}

	type Item struct {
		Code int32
		Name string
	}
	type ShopModel struct {
		Items    []Item
		PlayerId int32
	}

	state := new(ShopModel)

	resData, err := db.HGetAll("test_shop")
	fmt.Println(23423, resData)
	state.PlayerId = resData["PlayerId"].Int32()
	itemsJSON := resData["Items"].String()

	var items []Item
	err = json.Unmarshal([]byte(itemsJSON), &items)
	if err != nil {
		// 处理错误
		fmt.Println("JSON解码错误：", err)
		return
	}

	state.Items = items

	fmt.Printf("333,%+v, %d, %s \n", state, state.Items[0].Code, state.Items[1].Name)

}

func TestDBAppend(t *testing.T) {
	db, err := db.InfDb.NewDBClient(&db.SctSSdb{})
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}

	type Item struct {
		Code int32
		Name string
	}
	type ShopModel struct {
		Items    []Item
		PlayerId int32
	}

	// 从库中获取原始数据
	resData, err := db.HGetAll("test_shop")
	if err != nil {
		// 处理错误
		fmt.Println("HGetAll 错误：", err)
		return
	}

	// 解析原始数据为 ShopModel 结构
	state := new(ShopModel)
	state.PlayerId = resData["PlayerId"].Int32()

	itemsJSON := resData["Items"].String()

	var items []Item
	err = json.Unmarshal([]byte(itemsJSON), &items)
	if err != nil {
		// 处理错误
		fmt.Println("JSON解码错误：", err)
		return
	}

	state.Items = items

	// 向 Items 切片中添加新值
	newItem := Item{Code: 3, Name: "Item 3"}
	state.Items = append(state.Items, newItem)

	// 将更新后的 ShopModel 结构转换为 JSON 字符串
	updatedItemsJSON, err := json.Marshal(state.Items)
	if err != nil {
		// 处理错误
		fmt.Println("JSON编码错误：", err)
		return
	}

	stateData := map[string]interface{}{
		"PlayerId": state.PlayerId,
		"Items":    string(updatedItemsJSON),
	}

	// 使用 Hmset 方法将更新后的数据存储回库中
	err = db.MultiHSet("test_shop", stateData)
	if err != nil {
		fmt.Println("Hmset 错误：", err)
		return
	}

}
