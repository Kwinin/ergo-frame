package player

import (
	"gamer/apps/gamerapp/player/mod"
	"gamer/common"
	"gamer/helper"
	"gamer/log"
	pbAccount "gamer/proto/account"
	pbGamer "gamer/proto/gamer"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/proto"
	"time"

	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

type GateGenServer struct {
	common.GbVar
	gen.Server
	SendChan chan []byte
	//clientHandler  GateGenHandlerInterface
	clientHandlers map[int32]GateGenHandlerInterface
}

type GateCastMessage struct {
	PlayerId int32
	ModuleId int32
	MethodId int32
	Buf      []byte
}

func (gateGS *GateGenServer) Init(process *gen.ServerProcess, args ...etf.Term) error {
	log.Logger.Infof("Init (%v,%s): args %v ", process.Name(), process.Self(), args)
	baseMod := mod.NewBaseMod(gateGS.GbVar, process, gateGS.SendChan)
	gateGS.clientHandlers = make(map[int32]GateGenHandlerInterface)

	gateGS.clientHandlers[int32(pbGamer.MSG_GAMER_ATTR_MODULE)] = mod.NewAttrMod(gateGS.GbVar, baseMod)
	gateGS.clientHandlers[int32(pbGamer.MSG_GAMER_SHOP_MODULE)] = mod.NewShopMod(gateGS.GbVar, baseMod)

	process.SendAfter(process.Self(), etf.Atom("login"), time.Second)
	return nil
}

func (gateGS *GateGenServer) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("gateGen HandleCast (%v): %v", process.Name(), message)
	//defer func() {
	//	if err := recover(); err != nil {
	//		pc, fn, line, _ := runtime.Caller(5)
	//		log.Logger.Errorf("process:[%v] funcname:[%v] fn:[%v] line:[%v]", process.Name(), runtime.FuncForPC(pc).Name(), fn, line)
	//	}
	//}()

	msg := &GateCastMessage{}
	if err := etf.TermIntoStruct(message, msg); err != nil {
		log.Logger.Errorf("TermIntoStruct: %#v \n", err)
	}

	gateGS.clientHandlers[msg.ModuleId].MsgHandler(msg.PlayerId, msg.MethodId, msg.Buf)
	return gen.ServerStatusOK
}

func (gateGS *GateGenServer) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	log.Logger.Infof("HandleCall (%v): %v, From: %v", process.Name(), message, from)

	//gateGS.clientHander.HandleCall(message)
	reply := etf.Atom("ignore")
	return reply, gen.ServerStatusOK
}

func (gateGS *GateGenServer) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	switch info := message.(type) {
	case etf.Atom:
		switch info {
		case "login":
			//gateGS.SendChan <- []byte("send msg login ")
			gateGS.SendToClient(int32(pbAccount.MSG_ACCOUNT_MODULE), int32(pbAccount.MSG_ACCOUNT_LOGIN), &pbAccount.Msg_1001Rsp{Code: 1, Data: "ok"})
		}
	}

	return gen.ServerStatusOK
}

// Terminate called when process died
func (gateGS *GateGenServer) Terminate(process *gen.ServerProcess, reason string) {
	log.Logger.Infof("Terminate (%v): %v", process.Name(), reason)
}

func (gateGS *GateGenServer) SendToClient(module int32, method int32, pb proto.Message) {
	//logrus.Debugf("client send msg [%v] [%v] [%v]", module, method, pb)
	data, err := proto.Marshal(pb)
	if err != nil {
		logrus.Errorf("proto encode error[%v] [%v][%v] [%v]", err.Error(), module, method, pb)
		return
	}

	moduleBuf := helper.IntToBytes(module, 2)
	methodBuf := helper.IntToBytes(method, 2)
	gateGS.SendChan <- helper.BytesCombine(moduleBuf, methodBuf, data)
}
