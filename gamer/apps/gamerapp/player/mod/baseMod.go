package mod

import (
	"fmt"
	"gamer/common"
	"gamer/helper"
	"gamer/log"
	pbGamer "gamer/proto/gamer"
	"github.com/ergo-services/ergo/gen"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/proto"
	"runtime"
)

type BaseMod struct {
	common.GbVar
	InfoFunc map[int32]func(buf []byte)
	sendChan chan []byte
	process  *gen.ServerProcess
	PlayerId int32
}

func NewBaseMod(gbVar common.GbVar, process *gen.ServerProcess, sendChan chan []byte, playerId int32) BaseMod {
	client := BaseMod{
		GbVar:    gbVar,
		PlayerId: playerId,
		InfoFunc: make(map[int32]func(buf []byte)),
		process:  process,
		sendChan: sendChan,
	}

	client.initMsgRoute()

	//client.AddRoute(attr.infoFunc)
	return client
}

func (c *BaseMod) initMsgRoute() {
	//消息注册
	//账号
	//c.infoFunc[int32(pbGamer.MSG_GAMER_ATTR_INFO)] = helper.CreateRegisterFunc(c.attrInfo)
}

func (c *BaseMod) AddRoute(infoFunc map[int32]func(buf []byte)) {
	// 在此方法中，您可以直接操作 BaseMod 的 c.infoFunc
	for msgID, msgFunc := range infoFunc {
		c.InfoFunc[msgID] = msgFunc
	}
}

func (c *BaseMod) baseInfo(msg *pbGamer.Msg_2101Req) {

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

func (s *BaseMod) SendToClient(module int32, method int32, pb proto.Message) {
	//logrus.Debugf("client send msg [%v] [%v] [%v]", module, method, pb)
	data, err := proto.Marshal(pb)
	if err != nil {
		logrus.Errorf("proto encode error[%v] [%v][%v] [%v]", err.Error(), module, method, pb)
		return
	}

	mldulebuf := helper.IntToBytes(module, 2)
	methodbuf := helper.IntToBytes(method, 2)
	s.sendChan <- helper.BytesCombine(mldulebuf, methodbuf, data)
}

func (c *BaseMod) BeforeHandler() {

}
func (c *BaseMod) AfterHandler() {

}

func (c *BaseMod) MsgHandler(module, method int32, buf []byte) {
	defer func() {
		if err := recover(); err != nil {
			var err string
			for i := 0; i < 10; i++ {
				pc, fn, line, _ := runtime.Caller(i)
				if line == 0 {
					break
				}
				err += fmt.Sprintf("funcname:[%v] fn:[%v] line:[%v] \n", runtime.FuncForPC(pc).Name(), fn, line)
			}
			logrus.Error("err: \n", err)
		}
	}()

	//禁用模块
	//next...

	log.Logger.Infof("infoFunc %+v \n", c.InfoFunc)
	if msgFunc := c.InfoFunc[method]; msgFunc != nil {
		//if c.connectState == StatusGame {
		//	msgfunc(buf)
		//} else {
		//	logrus.Errorf("未登陆 [%v] [%v] [%v]", module, method, buf)
		//}
		msgFunc(buf)
	} else {
		log.Logger.Infof("未注册的消息", module, method)
	}
}

//type ModInf interface {
//	Name() string
//	OnDate() string
//}
//
//func (md *BaseMod) LoopMod() {
//	Names := []string{"Name"}
//	md.LoopModByMethods(Names)
//}
//
//func (md *BaseMod) LoopModByMethods(methods []string) {
//	mods := []ModInf{
//		mod.Attr{
//			common.GbVar{
//				NodeName: md.NodeName,
//				Cfg:      md.Cfg,
//				DB:       md.DB,
//			},
//		},
//		mod.Shop{
//			common.GbVar{
//				NodeName: md.NodeName,
//				Cfg:      md.Cfg,
//				DB:       md.DB,
//			},
//		},
//	}
//	for _, animal := range mods {
//		modValue := reflect.ValueOf(animal)
//		animalType := modValue.Type()
//
//		for i := 0; i < animalType.NumMethod(); i++ {
//			method := animalType.Method(i)
//			methodValue := modValue.MethodByName(method.Name)
//
//			if methodValue.IsValid() && helper.IsValueExists(method.Name, methods) {
//				result := methodValue.Call([]reflect.Value{})
//				log.Logger.Infof("result %+v", result)
//			}
//		}
//	}
//}
