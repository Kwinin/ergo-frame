package player

import (
	"gamer/common"
	"gamer/log"
	"time"

	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

type GateGenServer struct {
	common.GbVar
	gen.Server
	SendChan      chan []byte
	clientHandler GateGenHandlerInterface
}

func (gateGS *GateGenServer) Init(process *gen.ServerProcess, args ...etf.Term) error {
	log.Logger.Infof("Init (%v,%s): args %v ", process.Name(), process.Self(), args)
	//gateGS.clientHander = args[1].(GateGenHanderInterface)
	//gateGS.clientHander.InitHander(process, gateGS.sendChan)

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
	//
	//switch info := message.(type) {
	//case etf.Atom:
	//	switch info {
	//	case "SocketStop":
	//		return gen.ServerStatusStopWithReason("stop normal")
	//	case "timeloop":
	//		log.Logger.Debug("time loop")
	//	}
	//case etf.Tuple:
	//	module := info[0].(int32)
	//	method := info[1].(int32)
	//	buf := info[2].([]byte)
	//	log.Logger.Infof("module %v ,method %v, arg %v", module, method, buf)
	//	//gateGS.clientHander.MsgHander(module, method, buf)
	//	gateGS.SendChan <- []byte("send msg test")
	//case []byte:
	//	log.Logger.Debug("[]byte:", info)
	//}
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
			gateGS.SendChan <- []byte("send msg login ")
		}
	}

	return gen.ServerStatusOK
}

// Terminate called when process died
func (gateGS *GateGenServer) Terminate(process *gen.ServerProcess, reason string) {
	log.Logger.Infof("Terminate (%v): %v", process.Name(), reason)
	gateGS.clientHandler.Terminate(reason)
}