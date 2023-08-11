package masterapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"master/common"
	"master/log"
)

func createMasterActor() gen.ServerBehavior {
	return &MasterActor{}
}

type MasterActor struct {
	gen.Server
	CmdChan chan string
}

// Init invoked on a start this process.
func (s *MasterActor) Init(process *gen.ServerProcess, args ...etf.Term) error {
	s.CmdChan = args[0].(chan string)
	log.Logger.Infof("Init process: %s with name %q and args %v \n", process.Self(), process.Name(), args)
	//opts := gen.RemoteSpawnOptions{
	//	Name: "remote",
	//}
	//
	//gotPid, err := process.RemoteSpawn("WsGate@localhost", "remote", opts, 1, 2, 3)
	//if err != nil {
	//	fmt.Println(133, err)
	//}
	//fmt.Println("OK", process.Name(), process.Self(), gotPid)
	return nil
}

//
// Methods below are optional, so you can remove those that aren't be used
//

// HandleInfo invoked if this process received message sent with Process.Send(...).
func (s *MasterActor) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleInfo: %#v \n", message)
	return gen.ServerStatusOK
}

// HandleCast invoked if this process received message sent with ServerProcess.Cast(...).
// Return ServerStatusStop to stop server with "normal" reason. Use ServerStatus(error)
// for the custom reason
func (s *MasterActor) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleCast: %#v \n", message)
	return gen.ServerStatusOK
}

// HandleCall invoked if this process got sync request using ServerProcess.Call(...)
func (s *MasterActor) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	s.CmdChan <- string(common.Shutdown)
	log.Logger.Infof("HandleCall: %#v \n", message)

	return nil, gen.ServerStatusOK
}

// HandleDirect invoked on a direct request made with Process.Direct(...)
func (s *MasterActor) HandleDirect(process *gen.ServerProcess, ref etf.Ref, message interface{}) (interface{}, gen.DirectStatus) {
	log.Logger.Infof("HandleDirect: %#v \n", message)
	return nil, nil
}

// Terminate invoked on a termination process. ServerProcess.State is not locked during this callback.
func (s *MasterActor) Terminate(process *gen.ServerProcess, reason string) {
	log.Logger.Infof("Terminated: %s with reason %s", process.Self(), reason)
}
