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
	Items    []Item
	PlayerId int32
}

func newShopModel(playerId int32) *ShopModel {
	sm := new(ShopModel)
	sm.PlayerId = playerId
	return sm
}

func NewShopModel(playerId int32) *ShopModel {
	return newShopModel(playerId)
}

func (sm *ShopModel) TableName() string {
	return fmt.Sprintf("state_model:%d", sm.PlayerId)

}

func (sm *ShopModel) SetAllShop(db *db.DBClient) error {
	// 将 Items 切片转化为JSON字符串
	itemsJSON, err := json.Marshal(sm.Items)
	if err != nil {
		return fmt.Errorf("JSON Marshal %v", err)
	}

	data := map[string]interface{}{
		"PlayerId": sm.PlayerId,
		"Items":    string(itemsJSON),
	}

	err = db.MultiHSet(sm.TableName(), data)
	if err != nil {
		return fmt.Errorf("MultiHSet error %v", err)
	}
	return nil
}

func (sm *ShopModel) GetAllShop(db *db.DBClient) (*ShopModel, error) {
	resData, err := db.HGetAll(sm.TableName())
	if err != nil {
		return nil, err
	}
	shop := new(ShopModel)
	shop.PlayerId = resData["PlayerId"].Int32()
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
