package cmd

import (
	"fmt"
	"master/common"
	"master/config"
	"master/log"

	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
)

type ErgoPing struct {
	NodeName       string
	ToServerName   string
	ToServerId     int32
	genServerName  string
	gateNodeName   string
	debugGenServer *DebugGenServer
}

func NewDebugGen(nodeName, toServerName string, toServerId int32) *ErgoPing {
	gateNode, err := config.GetNodeInfo(toServerName, toServerId)
	if err != nil {
		log.Logger.Errorf("startDebugGen: %v", err)
	}

	newGen := &ErgoPing{
		NodeName:      nodeName,
		ToServerName:  toServerName,
		ToServerId:    toServerId,
		genServerName: fmt.Sprintf("%s_%d_actor", toServerName, toServerId),
		gateNodeName:  gateNode.Addr,
	}
	newGen.start()
	return newGen

}

func (er *ErgoPing) start() (node.Node, gen.Process) {

	lis := node.Listener{
		ListenBegin: uint16(config.ServerCfg.ListenBegin),
		ListenEnd:   uint16(config.ServerCfg.ListenEnd),
	}
	opts := node.Options{
		Listeners: []node.Listener{lis},
	}
	DebugNode, _ := ergo.StartNode(fmt.Sprintf("%s@localhost", er.NodeName), config.ServerCfg.Cookie, opts)

	log.Logger.Infof("DebugNode.ProxyRoutes,%+v", DebugNode.ProxyRoutes())

	er.debugGenServer = &DebugGenServer{}
	// Spawn supervisor process
	process, _ := DebugNode.Spawn(fmt.Sprintf("%s_debugGen", er.NodeName), gen.ProcessOptions{}, er.debugGenServer)

	log.Logger.Infof("Start Local Node: %s, GenServer: %s successfully", DebugNode.Name(), process.Name())

	return DebugNode, process
}

func (er *ErgoPing) Call(cmd string) (etf.Term, error) {

	msg := common.TransMessage{
		Msg:           "test",
		CMD:           cmd,
		FromNode:      config.ServerCfg.Node,
		FromGenServer: er.genServerName,
	}

	log.Logger.Infof("call node -> %v,%v, cmd: %v", er.genServerName, er.gateNodeName, msg)

	//return er.debugGenServer.process.Call(gen.ProcessID{Name: "stop_debugGen", Node: "stop@localhost"}, msg)
	//return er.debugGenServer.process.Call(gen.ProcessID{Name: "master_1_actor", Node: "Master@localhost"}, msg)
	return er.debugGenServer.process.Call(gen.ProcessID{Name: er.genServerName, Node: er.gateNodeName}, msg)
}

func (er *ErgoPing) Send(cmd string) error {

	msg := common.TransMessage{
		Msg: "test",
		CMD: cmd,
		FromNode: config.NodeConf{
			Id:   config.ServerCfg.Node.Id,
			Role: config.ServerCfg.Node.Role,
			Name: config.ServerCfg.Node.Name,
			Addr: config.ServerCfg.Node.Addr,
			Ip:   config.ServerCfg.Node.Ip,
		},
	}

	log.Logger.Infof("call node -> %v,%v, cmd: %v", er.genServerName, er.gateNodeName, msg)

	return er.debugGenServer.process.Send(gen.ProcessID{Name: er.genServerName, Node: er.gateNodeName}, msg)

}

func (er *ErgoPing) Direct(cmd string) (interface{}, error) {

	msg := common.TransMessage{
		Msg: "test",
		CMD: cmd,
		FromNode: config.NodeConf{
			Id:   config.ServerCfg.Node.Id,
			Role: config.ServerCfg.Node.Role,
			Name: config.ServerCfg.Node.Name,
			Addr: config.ServerCfg.Node.Addr,
			Ip:   config.ServerCfg.Node.Ip,
		},
	}

	log.Logger.Infof("call node -> %v,%v, cmd: %v", er.genServerName, er.gateNodeName, msg)

	return er.debugGenServer.process.Direct(msg)

}

func (er *ErgoPing) Ping() (bool, string) {
	server, err := er.Call("ping")
	if err != nil {
		fmt.Println(err)
		return false, ""
	}
	return true, fmt.Sprint(server)

}

func (er *ErgoPing) Monitor() {
	log.Logger.Infof("Monitor to -> Name: %s, Node: %s", er.genServerName, er.gateNodeName)
	mons := er.debugGenServer.process.MonitorProcess(gen.ProcessID{Name: er.genServerName, Node: er.gateNodeName})
	Pids := er.debugGenServer.process.MonitorsByName()
	IsMons := er.debugGenServer.process.IsMonitor(mons)
	log.Logger.Infof("%+v, %v", Pids, IsMons)
	for _, v := range Pids {

		log.Logger.Infof("%+v", v)
	}

	er.debugGenServer.process.Wait()
}

// GenServer implementation structure
type DebugGenServer struct {
	gen.Server
	process *gen.ServerProcess
}

// Init initializes process state using arbitrary arguments
// Init(...) -> state
func (dgs *DebugGenServer) Init(process *gen.ServerProcess, args ...etf.Term) error {
	dgs.process = process
	return nil
}

// HandleCast serves incoming messages sending via gen_server:cast
// HandleCast -> ("noreply", state) - noreply
//
//	("stop", reason) - stop with reason
func (dgs *DebugGenServer) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("DebugGenServer  HandleCast pName: %s, pNodeName: %s, message: %s ", process.Name(), process.NodeName(), message)
	switch m := message.(type) {
	case common.TransMessage:
		log.Logger.Info(111, m.Msg)
	case etf.Term:
		log.Logger.Info(222, m)
	default:
		log.Logger.Info(333, m)
	}
	return gen.ServerStatusOK
}

// HandleCall serves incoming messages sending via gen_server:call
// HandleCall -> ("reply", message, state) - reply
//
//			 ("noreply", _, state) - noreply
//	         ("stop", reason, _) - normal stop
func (dgs *DebugGenServer) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	log.Logger.Infof("DebugGenServer  HandleCall pName: %s, pNodeName: %s, message: %s ", process.Name(), process.NodeName(), message)
	switch m := message.(type) {
	case common.TransMessage:
		log.Logger.Info(111, m.Msg)
	case etf.Term:
		log.Logger.Info(222, m)
	default:
		log.Logger.Info(333, m)
	}

	return etf.Term(""), gen.ServerStatusOK
}

// HandleInfo serves all another incoming messages (Pid ! message)
// HandleInfo -> ("noreply", state) - noreply
//
//	("stop", reason) - normal stop
func (dgs *DebugGenServer) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("DebugGenServer  HandleInfo pName: %s, pNodeName: %s, message: %s ", process.Name(), process.NodeName(), message)

	switch m := message.(type) {
	case common.TransMessage:
		log.Logger.Info(111, m.Msg)
	case etf.Term:
		log.Logger.Info(222, m)
	default:
		log.Logger.Info(333, m)
	}

	Pids := dgs.process.MonitorsByName()
	log.Logger.Infof("%+v", Pids)
	for _, v := range Pids {

		log.Logger.Infof("%+v", v)
	}
	return gen.ServerStatusOK
}

// HandleDirect invoked on a direct request made with Process.Direct(...)
func (dgs *DebugGenServer) HandleDirect(process *gen.ServerProcess, ref etf.Ref, message interface{}) (interface{}, gen.DirectStatus) {
	log.Logger.Infof("HandleDirect: %#v \n", message)
	switch m := message.(type) {
	case common.TransMessage:
		log.Logger.Info(111, m.Msg)
	case etf.Term:
		log.Logger.Info(222, m)
	default:
		log.Logger.Info(333, m)
	}

	return nil, nil
}

// Terminate called when process died
func (dgs *DebugGenServer) Terminate(process *gen.ServerProcess, reason string) {
	log.Logger.Infof("DebugGenServer Terminate pName: %s, pNodeName: %s, Reason: %s ", process.Name(), process.NodeName(), reason)
}
