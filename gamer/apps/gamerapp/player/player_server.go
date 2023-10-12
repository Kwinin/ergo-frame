package player

import (
	"fmt"
	"gamer/helper"
	pbGamer "gamer/proto/gamer"
	"github.com/ergo-services/ergo/gen"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
	"runtime"
)

type PlayerServer struct {
	infoFunc map[int32]func(buf []byte)
	sendChan chan []byte
	process  *gen.ServerProcess
}

func NewPlayerServer() *PlayerServer {
	client := &PlayerServer{}
	client.initMsgRoute()
	return client
}

func (c *PlayerServer) initMsgRoute() {
	//消息注册
	c.infoFunc = make(map[int32]func(buf []byte))
	//账号
	c.infoFunc[int32(pbGamer.MSG_GAMER_ATTR_INFO)] = createRegisterFunc(c.attrInfo)

}

func (c *PlayerServer) attrInfo(msg *pbGamer.Msg_2101Req) {

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

func (s *PlayerServer) SendToClient(module int32, method int32, pb proto.Message) {
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

func (c *PlayerServer) InitHandler(process *gen.ServerProcess, sendChan chan []byte) {
	c.process = process
	c.sendChan = sendChan
}

func (c *PlayerServer) MsgHandler(module, method int32, buf []byte) {
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

	if msgFunc := c.infoFunc[method]; msgFunc != nil {
		//if c.connectState == StatusGame {
		//	msgfunc(buf)
		//} else {
		//	logrus.Errorf("未登陆 [%v] [%v] [%v]", module, method, buf)
		//}
		msgFunc(buf)
	} else {
		logrus.Warnln("未注册的消息", module, method)
	}
}

// ==========msg register =======
// 消息注册
func createRegisterFunc[T any](execfunc func(*T)) func(buf []byte) {
	return func(buf []byte) {
		info := new(T)
		err := decodeProto(info, buf)
		if err != nil {
			logrus.Errorf("decode error[%v]", err.Error())
		} else {
			//logrus.Debugf("client msg:[%v] [%v]", info, tools.GoID())
			execfunc(info)
		}
	}
}

// protobuf 解码
func decodeProto(info interface{}, buf []byte) error {
	if data, ok := info.(protoreflect.ProtoMessage); ok {
		return proto.Unmarshal(buf, data)
	}
	return nil
}

//type ModInf interface {
//	Name() string
//	OnDate() string
//}
//
//func (md *PlayerServer) LoopMod() {
//	Names := []string{"Name"}
//	md.LoopModByMethods(Names)
//}
//
//func (md *PlayerServer) LoopModByMethods(methods []string) {
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
