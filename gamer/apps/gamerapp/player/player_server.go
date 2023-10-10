package player

import (
	"fmt"
	"gamer/apps/gamerapp/player/mod"
	"gamer/common"
	"gamer/helper"
	"gamer/log"
	pbaccount "gamer/proto/account"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
	"reflect"
)

type Server struct {
	common.GbVar
	infoFunc map[int]func(buf []byte)
	sendChan chan []byte
}

func NewServer() *Server {
	client := &Server{}
	client.initMsgRoute()
	return client
}

func (c *Server) initMsgRoute() {
	//消息注册
	c.infoFunc = make(map[int]func(buf []byte))
	//账号
	c.infoFunc[int(pbaccount.MSG_ACCOUNT_LOGIN)] = createRegisterFunc(c.accountLogin)

}

func (c *Server) accountLogin(msg *pbaccount.Msg_1001Req) {

	fmt.Println(1111, msg.Account, msg.Password)

}

func (s *Server) SendToClient(module int32, method int32, pb proto.Message) {
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

type ModInf interface {
	Name() string
	OnDate() string
}

func (md *Server) LoopMod() {
	Names := []string{"Name"}
	md.LoopModByMethods(Names)
}

func (md *Server) LoopModByMethods(methods []string) {
	mods := []ModInf{
		mod.Attr{
			common.GbVar{
				NodeName: md.NodeName,
				Cfg:      md.Cfg,
				DB:       md.DB,
			},
		},
		mod.Shop{
			common.GbVar{
				NodeName: md.NodeName,
				Cfg:      md.Cfg,
				DB:       md.DB,
			},
		},
	}
	for _, animal := range mods {
		modValue := reflect.ValueOf(animal)
		animalType := modValue.Type()

		for i := 0; i < animalType.NumMethod(); i++ {
			method := animalType.Method(i)
			methodValue := modValue.MethodByName(method.Name)

			if methodValue.IsValid() && helper.IsValueExists(method.Name, methods) {
				result := methodValue.Call([]reflect.Value{})
				log.Logger.Infof("result %+v", result)
			}
		}
	}
}
