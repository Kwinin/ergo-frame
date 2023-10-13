package mod

import (
	"fmt"
	"gamer/common"
	"gamer/helper"
	"gamer/log"
	pbGamer "gamer/proto/gamer"
	"runtime"
	"time"
)

type Shop struct {
	common.GbVar
	baseMod
}

func NewShopMod(gbVar common.GbVar, baseMod baseMod) *Shop {
	shop := &Shop{
		GbVar:   gbVar,
		baseMod: baseMod,
	}

	shop.initMsgRoute()
	return shop
}
func (c *Shop) initMsgRoute() {
	//消息注册
	//账号
	c.infoFunc[int32(pbGamer.MSG_GAMER_SHOP_INFO)] = helper.CreateRegisterFunc(c.shopInfo)
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
	rspMsg := &pbGamer.Msg_2201Rsp{
		Items: []*pbGamer.Item{
			{
				Code: 10000234,
				Name: "屠龙刀",
			},
			{
				Code: 10000235,
				Name: "倚天剑",
			},
		},
	}
	c.sendToClient(int32(pbGamer.MSG_GAMER_SHOP_MODULE), int32(pbGamer.MSG_GAMER_SHOP_INFO), rspMsg)
}
