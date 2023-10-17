package player

import (
	"fmt"
	"gamer/apps/gamerapp/player/mod"
	"gamer/apps/gamerapp/player/mod/shop"
	"gamer/common"
	"gamer/helper"
	"gamer/log"
	pbAccount "gamer/proto/account"
	pbGamer "gamer/proto/gamer"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/proto"
	"strconv"
	"time"

	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

type PlayerGenServer struct {
	common.GbVar
	gen.Server
	SendChan chan []byte
	//clientHandler  GateGenHandlerInterface
	clientHandlers map[int32]PlayerHandlerInterface
}

type GateCastMessage struct {
	PlayerId int32
	ModuleId int32
	MethodId int32
	Buf      []byte
}

func (gs *PlayerGenServer) modMethodRegister(process *gen.ServerProcess, playerId int32) {
	baseMod := mod.NewBaseMod(gs.GbVar, process, gs.SendChan, playerId)
	gs.clientHandlers = make(map[int32]PlayerHandlerInterface)

	gs.clientHandlers[int32(pbGamer.MSG_GAMER_ATTR_MODULE)] = mod.NewAttrMod(gs.GbVar, baseMod)
	gs.clientHandlers[int32(pbGamer.MSG_GAMER_SHOP_MODULE)] = shop.NewShopMod(gs.GbVar, baseMod)
}
func (gs *PlayerGenServer) login() {

}
func (gs *PlayerGenServer) Init(process *gen.ServerProcess, args ...etf.Term) error {
	log.Logger.Infof("Init (%v,%s): args %v ", process.Name(), process.Self(), args)
	switch info := args[0].(type) {
	case etf.Atom:
		num, err := strconv.ParseInt(string(info), 10, 32)
		if err != nil {
			log.Logger.Info("转换失败:", err)
		} else {
			gs.modMethodRegister(process, int32(num))
		}

	default:
		fmt.Println(333)
	}

	gs.login()

	process.SendAfter(process.Self(), etf.Atom("login"), time.Second)
	return nil
}

func (gs *PlayerGenServer) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
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

	gs.clientHandlers[msg.ModuleId].MsgHandler(msg.PlayerId, msg.MethodId, msg.Buf)
	return gen.ServerStatusOK
}

func (gs *PlayerGenServer) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	log.Logger.Infof("HandleCall (%v): %v, From: %v", process.Name(), message, from)

	//gs.clientHander.HandleCall(message)
	reply := etf.Atom("ignore")
	return reply, gen.ServerStatusOK
}

func (gs *PlayerGenServer) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	switch info := message.(type) {
	case etf.Atom:
		switch info {
		case "login":
			//gs.SendChan <- []byte("send msg login ")
			gs.SendToClient(int32(pbAccount.MSG_ACCOUNT_MODULE), int32(pbAccount.MSG_ACCOUNT_LOGIN), &pbAccount.Msg_1001Rsp{Code: 1, Data: "ok"})
		}
	}

	return gen.ServerStatusOK
}

// Terminate called when process died
func (gs *PlayerGenServer) Terminate(process *gen.ServerProcess, reason string) {
	log.Logger.Infof("Terminate (%v): %v", process.Name(), reason)
	for _, allMods := range gs.clientHandlers {
		allMods.AfterHandler()
	}
}

func (gs *PlayerGenServer) SendToClient(module int32, method int32, pb proto.Message) {
	//logrus.Debugf("client send msg [%v] [%v] [%v]", module, method, pb)
	data, err := proto.Marshal(pb)
	if err != nil {
		logrus.Errorf("proto encode error[%v] [%v][%v] [%v]", err.Error(), module, method, pb)
		return
	}

	moduleBuf := helper.IntToBytes(module, 2)
	methodBuf := helper.IntToBytes(method, 2)
	gs.SendChan <- helper.BytesCombine(moduleBuf, methodBuf, data)
}
