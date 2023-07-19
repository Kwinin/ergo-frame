package wsgateapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func createWsGateActor() gen.ServerBehavior {
	return &WsGateActor{}
}

type WsGateActor struct {
	gen.Server
}

// Init invoked on a start this process.
func (s *WsGateActor) Init(process *gen.ServerProcess, args ...etf.Term) error {
	logger.Infof("Init process: %s with name %q and args %v \n", process.Self(), process.Name(), args)
	return nil
}

//
// Methods below are optional, so you can remove those that aren't be used
//

// HandleInfo invoked if this process received message sent with Process.Send(...).
func (s *WsGateActor) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	logger.Infof("HandleInfo: %#v \n", message)
	return gen.ServerStatusOK
}

// HandleCast invoked if this process received message sent with ServerProcess.Cast(...).
// Return ServerStatusStop to stop server with "normal" reason. Use ServerStatus(error)
// for the custom reason
func (s *WsGateActor) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	logger.Infof("HandleCast: %#v \n", message)
	return gen.ServerStatusOK
}

// HandleCall invoked if this process got sync request using ServerProcess.Call(...)
func (s *WsGateActor) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	logger.Infof("HandleCall: %#v \n", message)
	return nil, gen.ServerStatusOK
}

// HandleDirect invoked on a direct request made with Process.Direct(...)
func (s *WsGateActor) HandleDirect(process *gen.ServerProcess, ref etf.Ref, message interface{}) (interface{}, gen.DirectStatus) {
	logger.Infof("HandleDirect: %#v \n", message)
	return nil, nil
}

// Terminate invoked on a termination process. ServerProcess.State is not locked during this callback.
func (s *WsGateActor) Terminate(process *gen.ServerProcess, reason string) {
	logger.Infof("Terminated: %s with reason %s", process.Self(), reason)
}
