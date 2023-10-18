package shop

import (
	"fmt"
	"gamer/apps/gamerapp/player/mod"
	"gamer/common"
	"gamer/helper"
	"gamer/log"
	pbGamer "gamer/proto/gamer"
	"runtime"
	"time"
)

type Shop struct {
	common.GbVar
	mod.BaseMod
	model *ShopModel
}

func NewShopMod(gbVar common.GbVar, BaseMod mod.BaseMod) *Shop {
	shop := &Shop{
		GbVar:   gbVar,
		BaseMod: BaseMod,
		model:   NewShopModel(),
	}
	shop.initHandler()
	shop.initMsgRoute()
	return shop
}

func (c *Shop) initMsgRoute() {
	//消息注册
	//账号
	c.InfoFunc[int32(pbGamer.MSG_GAMER_SHOP_INFO)] = helper.CreateRegisterFunc(c.shopInfo)
	c.InfoFunc[int32(pbGamer.MSG_GAMER_SHOP_ADD)] = helper.CreateRegisterFunc(c.addShopItem)
}
func (c *Shop) initHandler() {
	load, err := c.OnLoad()
	if err != nil {
		log.Logger.Error(err)
	}
	log.Logger.Infof("Load shop mod, playerId:%d, shopModel: %+v", c.PlayerId, load)

}

func (c *Shop) BeforeHandler() {

}

func (c *Shop) AfterHandler() {

}
func (c *Shop) OfflineHandler() {
	err := c.model.SetAllShop(c.DB, c.PlayerId)
	if err != nil {
		log.Logger.Error(err)
	}
}

func (s *Shop) Name() string {
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		log.Logger.Infof("当前文件名:%s", filename)
	} else {
		log.Logger.Info("无法获取当前文件名")
	}

	result, _ := s.DB.Get("my_key")

	log.Logger.Infof("******* %v", result)
	return filename
}

func (a *Shop) OnDate() string {
	return time.Now().String()
}

func (a *Shop) OnMessage(Id int) string {
	return fmt.Sprintf("use id :%d", Id)
}

func (c *Shop) shopInfo(msg *pbGamer.Msg_2201Req) {
	log.Logger.Infof("shopInfo %+v", msg)

	rspMsg := &pbGamer.Msg_2201Rsp{Items: []*pbGamer.Item{}}
	for _, item := range c.model.Items {
		rspMsg.Items = append(rspMsg.Items, &pbGamer.Item{Code: item.Code, Name: item.Name})
	}
	rspMsg.RetCode = 1
	c.SendToClient(int32(pbGamer.MSG_GAMER_SHOP_MODULE), int32(pbGamer.MSG_GAMER_SHOP_INFO), rspMsg)
}

func (c *Shop) addShopItem(msg *pbGamer.Msg_2202Req) {
	log.Logger.Infof(" addShopItem %+v", msg)

	newItem := Item{Code: msg.ItemCode, Name: msg.ItemName}
	c.model.Items = append(c.model.Items, newItem)
	rspMsg := &pbGamer.Msg_2202Rsp{RetCode: 1}
	c.SendToClient(int32(pbGamer.MSG_GAMER_SHOP_MODULE), int32(pbGamer.MSG_GAMER_SHOP_ADD), rspMsg)
}

func (c *Shop) OnLoad() (*ShopModel, error) {
	m, err := c.model.GetAllShop(c.GbVar.DB, c.PlayerId)
	if err != nil {
		return nil, err
	}
	c.model = m
	return c.model, nil
}
