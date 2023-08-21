package gamerapp

import (
	"gamer/common"
	"gamer/log"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func createGamerActor() gen.ServerBehavior {
	return &GamerActor{}
}

type GamerActor struct {
	gen.Server
}

type makeCall struct {
	to      interface{}
	message interface{}
}

// Init invoked on a start this process.
func (s *GamerActor) Init(process *gen.ServerProcess, args ...etf.Term) error {
	log.Logger.Infof("Init GamerActor process: %s with name %q and args %v", process.Self(), process.Name(), args)
	//message := etf.Tuple{etf.Atom("Kwinin Hello")}
	//
	//call := makeCall{
	//	to:      process.Self(),
	//	message: message,
	//}
	//result, e := process.Direct(call)
	//if e != nil {
	//	fmt.Println(e)
	//}
	//fmt.Println(result)
	return nil
}

//
// Methods below are optional, so you can remove those that aren't be used
//

// HandleInfo invoked if this process received message sent with Process.Send(...).
func (s *GamerActor) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleInfo: %#v \n", message)
	return gen.ServerStatusOK
}

// HandleCast invoked if this process received message sent with ServerProcess.Cast(...).
// Return ServerStatusStop to stop server with "normal" reason. Use ServerStatus(error)
// for the custom reason
func (s *GamerActor) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleCast: %#v \n", message)
	return gen.ServerStatusOK
}

// HandleCall invoked if this process got sync request using ServerProcess.Call(...)
func (s *GamerActor) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	log.Logger.Infof("HandleCall: %#v \n", message)

	msg := &common.TransMessage{}
	if err := etf.TermIntoStruct(message, msg); err != nil {
		log.Logger.Errorf("TermIntoStruct: %#v \n", err)
	}

	log.Logger.Infof("TransMessage, %+v", msg)

	return nil, gen.ServerStatusOK
}

// HandleDirect invoked on a direct request made with Process.Direct(...)
func (s *GamerActor) HandleDirect(process *gen.ServerProcess, ref etf.Ref, message interface{}) (interface{}, gen.DirectStatus) {
	log.Logger.Infof("HandleDirect: %#v \n", message)
	return nil, nil
}

// Terminate invoked on a termination process. ServerProcess.State is not locked during this callback.
func (s *GamerActor) Terminate(process *gen.ServerProcess, reason string) {
	log.Logger.Infof("Terminated: %s with reason %s", process.Self(), reason)
}
