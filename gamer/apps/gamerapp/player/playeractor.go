package player

import (
	"gamer/common"
	"gamer/log"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"time"
)

func createPlayerActor(gbVar common.GbVar) gen.ServerBehavior {
	return &Actor{
		GbVar: common.GbVar{
			NodeName: gbVar.NodeName,
			Cfg:      gbVar.Cfg,
			DB:       gbVar.DB,
		},
	}
}

type Actor struct {
	gen.Server
	common.GbVar
}

func (s *Actor) LaunchPid(PlayerId, ServerId int) {
	log.Logger.Infof("Kwinin  LaunchPid %d, %d", PlayerId, ServerId)
}

// Init invoked on a start this process.
func (s *Actor) Init(process *gen.ServerProcess, args ...etf.Term) error {
	log.Logger.Infof("Init Player process: %s with name %q and args %+v \n", process.Self(), process.Name(), args)
	return nil
}

//
// Methods below are optional, so you can remove those that aren't be used
//

// HandleInfo invoked if this process received message sent with Process.Send(...).
func (s *Actor) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleInfo: %#v \n", message)
	return gen.ServerStatusOK
}

// HandleCast invoked if this process received message sent with ServerProcess.Cast(...).
// Return ServerStatusStop to stop server with "normal" reason. Use ServerStatus(error)
// for the custom reason
func (s *Actor) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleCast: %#v \n", message)
	return gen.ServerStatusOK
}

// HandleCall invoked if this process got sync request using ServerProcess.Call(...)
func (s *Actor) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	//msg := &Message{}
	//if err := etf.TermIntoStruct(message, msg); err != nil {
	//	log.Logger.Errorf("TermIntoStruct: %#v \n", err)
	//}
	//fmt.Println(msg, msg.Code)
	//switch msg.Code {
	//case 1:
	//	processName := process.Name()
	//	process.Exit(fmt.Sprintf("%s/%s 已经停止", process.Self().String(), processName))
	//
	//	return msg.Account, gen.ServerStatusOK
	//case 2:
	//	name := fmt.Sprintf("%s_%d", "player", msg.Account)
	//
	//	p2 := process.ProcessByName(name)
	//	fmt.Printf("p2= %v , p1= %v \n", p2.Self(), process.Self())
	//
	//	//_, err := process.Call(p2.Self(), msg.Data)
	//	//if err != nil {
	//	//	log.Logger.Errorf("p2 Direct %v", err)
	//	//}
	//
	//	//call := makeCall{
	//	//	to:      p2,
	//	//	message: msg.Data,
	//	//}
	//	//_, err := p2.Direct(call)
	//	//if err != nil {
	//	//	log.Logger.Errorf("p2 Direct %v", err)
	//	//}
	//
	//	//server := &Server{GbVar: common.GbVar{
	//	//	NodeName: s.NodeName,
	//	//	Cfg:      s.Cfg,
	//	//	DB:       s.DB}}
	//	//err := server.Hello(p2)
	//
	//	//NewServer()
	//
	//}
	//
	//log.Logger.Infof("HandleCall:  %+v, %+v \n", message, msg.Data)

	return "kwinin", gen.ServerStatusOK
}

// HandleDirect invoked on a direct request made with Process.Direct(...)
func (s *Actor) HandleDirect(process *gen.ServerProcess, ref etf.Ref, message interface{}) (interface{}, gen.DirectStatus) {
	log.Logger.Infof("HandleDirect: %#v \n", message)
	return nil, nil
}

// Terminate invoked on a termination process. ServerProcess.State is not locked during this callback.
func (s *Actor) Terminate(process *gen.ServerProcess, reason string) {
	log.Logger.Infof("Terminated: %s with reason %s", process.Self(), reason)
}

func OnLogin() {
	Sec := time.Now().String()
	log.Logger.Infof("login time %s \n", Sec)
}
