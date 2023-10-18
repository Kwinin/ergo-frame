package shop

import (
	"encoding/json"
	"fmt"
	"gamer/db"
)

type Item struct {
	Code int32
	Name string
}
type ShopModel struct {
	Items []Item
}

func newShopModel() *ShopModel {
	sm := new(ShopModel)
	return sm
}

func NewShopModel() *ShopModel {
	return newShopModel()
}

func (sm *ShopModel) TableName(playerId int32) string {
	return fmt.Sprintf("shop_model:%d", playerId)

}

func (sm *ShopModel) SetAllShop(db *db.DBClient, playerId int32) error {
	// 将 Items 切片转化为JSON字符串
	itemsJSON, err := json.Marshal(sm.Items)
	if err != nil {
		return fmt.Errorf("JSON Marshal %v", err)
	}

	data := map[string]interface{}{
		"Items": string(itemsJSON),
	}

	err = db.MultiHSet(sm.TableName(playerId), data)
	if err != nil {
		return fmt.Errorf("MultiHSet error %v", err)
	}
	return nil
}

func (sm *ShopModel) GetAllShop(db *db.DBClient, playerId int32) (*ShopModel, error) {
	resData, err := db.HGetAll(sm.TableName(playerId))
	if err != nil {
		return nil, err
	}
	shop := new(ShopModel)
	itemsJSON := resData["Items"].String()

	if itemsJSON != "" {
		var items []Item
		err = json.Unmarshal([]byte(itemsJSON), &items)
		if err != nil {
			return nil, fmt.Errorf("JSON Unmarshal %v", err)
		}
		shop.Items = items
	}

	return shop, nil
}
