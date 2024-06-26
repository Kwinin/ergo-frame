package attr

import (
	"gamer/apps/gamerapp/player/mod"
	"gamer/common"
	"gamer/helper"
	pbGamer "gamer/proto/gamer"
)

type Attr struct {
	common.GbVar
	mod.BaseMod
}

func NewAttrMod(gbVar common.GbVar, BaseMod mod.BaseMod) *Attr {
	attr := &Attr{
		GbVar:   gbVar,
		BaseMod: BaseMod,
	}

	attr.initMsgRoute()
	return attr
}

func (c *Attr) initMsgRoute() {
	//消息注册
	//账号
	c.InfoFunc[int32(pbGamer.MSG_GAMER_ATTR_INFO)] = helper.CreateRegisterFunc(c.attrInfo)
}

func (c *Attr) attrInfo(msg *pbGamer.Msg_2101Req) {

	rspMsg := &pbGamer.Msg_2101Rsp{
		Nickname: msg.Nickname,
		Address:  msg.Address,
		Addresses: []*pbGamer.Address{
			{
				Street: "成华大道",
				City:   "成都",
			},
			{
				Street: "二仙桥",
				City:   "成都",
			},
		},
	}
	c.SendToClient(int32(pbGamer.MSG_GAMER_ATTR_MODULE), int32(pbGamer.MSG_GAMER_ATTR_INFO), rspMsg)
}

//func (c *Attr) InitHandler(process *gen.ServerProcess, sendChan chan []byte) {
//	c.process = process
//	c.sendChan = sendChan
//}
//
//func (c *Attr) MsgHandler(module, method int32, buf []byte) {
//	defer func() {
//		if err := recover(); err != nil {
//			var err string
//			for i := 0; i < 10; i++ {
//				pc, fn, line, _ := runtime.Caller(i)
//				if line == 0 {
//					break
//				}
//				err += fmt.Sprintf("funcname:[%v] fn:[%v] line:[%v] \n", runtime.FuncForPC(pc).Name(), fn, line)
//			}
//			logrus.Error("err: \n", err)
//		}
//	}()
//
//	//禁用模块
//	//next...
//
//	fmt.Printf("InfoFunc %+v \n", c.InfoFunc)
//	if msgFunc := c.InfoFunc[method]; msgFunc != nil {
//		//if c.connectState == StatusGame {
//		//	msgfunc(buf)
//		//} else {
//		//	logrus.Errorf("未登陆 [%v] [%v] [%v]", module, method, buf)
//		//}
//		msgFunc(buf)
//	} else {
//		log.Logger.Infof("未注册的消息", module, method)
//	}
//}
