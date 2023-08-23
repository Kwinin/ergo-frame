package masterapp

import (
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"master/apps/masterapp/node"
	"master/common"
	"master/config"
	"master/db"
	"master/helper"
	"master/log"
)

func createMasterActor() gen.ServerBehavior {
	return &MasterActor{}
}

type MasterActor struct {
	gen.Server
	CmdChan chan string
	DB      *db.DBClient
}

// Init invoked on a start this process.
func (s *MasterActor) Init(process *gen.ServerProcess, args ...etf.Term) error {
	s.CmdChan = args[0].(chan string)
	s.DB = args[1].(*db.DBClient)
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
	//fmt.Println(34324, message, common.Shutdown, message == etf.Atom(common.Shutdown))
	//fmt.Println(34324, message, common.Shutdown, message.(string) == common.Shutdown)

	log.Logger.Infof("HandleCall: %#v \n", message)

	msg := &common.TransMessage{}
	if err := etf.TermIntoStruct(message, msg); err != nil {
		log.Logger.Errorf("TermIntoStruct: %#v \n", err)
	}

	if msg.From == config.ServerCfg.Node {
		// self
		s.CmdChan <- msg.CMD

	} else {
		nodeConf, err := config.GetNodeInfo(msg.From.Role, msg.From.Id)
		if err != nil {
			log.Logger.Errorf("%s, node found faild", msg.From.Name)
			return nil, gen.ServerStatusOK
		}
		log.Logger.Infof("get one from local node list %+v", nodeConf)
		switch true {
		case msg.From == *nodeConf:
			if helper.IsValueExists(msg.From.Role, config.ServerCfg.ConnectRoles) {
				nd := node.NewNodesModel()

				newNode := node.NodesModel{
					Id:     msg.From.Id,
					Role:   msg.From.Role,
					Name:   msg.From.Name,
					Addr:   msg.From.Addr,
					Status: common.Enable,
				}

				err := nd.SetOneNode(s.DB, newNode)
				if err != nil {
					log.Logger.Errorf("db op err: %v", err)
					return fmt.Sprintf("connect %s failed", config.ServerCfg.Node.Name), gen.ServerStatusOK
				}

				log.Logger.Infof("%s, registered successfully", msg.From.Name)
				return fmt.Sprintf("connect %s successfully", config.ServerCfg.Node.Name), gen.ServerStatusOK
			} else {
				log.Logger.Errorf("check role connect failed %s isn't exist %s", msg.From.Role, config.ServerCfg.ConnectRoles)
			}

		default:
			return nil, gen.ServerStatusOK
		}

	}

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
