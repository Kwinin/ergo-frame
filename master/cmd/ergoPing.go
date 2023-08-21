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

var (
	genServerName  string = fmt.Sprintf("%s_%d_actor", config.ServerCfg.ServerName, config.ServerCfg.ServerID)
	gateNodeName   string
	debugGenServer *DebugGenServer
)

func call(serverName string, serverId int32, cmd string) (etf.Term, error) {

	msg := common.TransMessage{
		CMD:  cmd,
		From: config.ServerCfg.Node,
	}

	if config.ServerCfg.ServerName != serverName {
		genServerName = fmt.Sprintf("%s_%d_actor", serverName, serverId)
		msg = common.TransMessage{
			CMD: cmd,
			From: config.NodeConf{
				Name: config.ServerCfg.Node.Name,
				Addr: config.ServerCfg.Node.Addr,
				Ip:   config.ServerCfg.Node.Ip,
			},
		}
	}
	log.Logger.Infof("call node -> %v,%v, cmd: %v", genServerName, gateNodeName, msg)

	return debugGenServer.process.Call(gen.ProcessID{Name: genServerName, Node: gateNodeName}, msg)

}

func send(cmd ...string) error {
	if len(cmd) == 1 {
		return debugGenServer.process.Send(gen.ProcessID{Name: genServerName, Node: gateNodeName}, etf.Atom(cmd[0]))
	} else {
		return debugGenServer.process.Send(gen.ProcessID{Name: genServerName, Node: gateNodeName}, cmd)
	}
}

func ping(serverName string, serverId int32) (bool, string) {
	startDebugGen(serverName, serverId)
	server, err := call(serverName, serverId, "ping")
	if err != nil {
		fmt.Println(err)
		return false, ""
	}
	return true, fmt.Sprint(server)

}

func startDebugGen(serverName string, serverId int32) (node.Node, gen.Process) {
	gateNode, err := config.GetNodeInfo(serverName, serverId)
	gateNodeName = gateNode.Addr
	if err != nil {
		log.Logger.Errorf("startDebugGen: %v", err)
	}

	lis := node.Listener{
		ListenBegin: uint16(config.ServerCfg.ListenBegin),
		ListenEnd:   uint16(config.ServerCfg.ListenEnd),
	}
	opts := node.Options{
		Listeners: []node.Listener{lis},
	}
	DebugNode, _ := ergo.StartNode("debug_server@localhost", config.ServerCfg.Cookie, opts)

	log.Logger.Infof("DebugNode.ProxyRoutes,%+v", DebugNode.ProxyRoutes())

	debugGenServer = &DebugGenServer{}
	// Spawn supervisor process
	process, _ := DebugNode.Spawn("deubg_gen", gen.ProcessOptions{}, debugGenServer)

	return DebugNode, process
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
	return gen.ServerStatusOK
}

// HandleCall serves incoming messages sending via gen_server:call
// HandleCall -> ("reply", message, state) - reply
//
//			 ("noreply", _, state) - noreply
//	         ("stop", reason, _) - normal stop
func (dgs *DebugGenServer) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	return etf.Term(""), gen.ServerStatusOK
}

// HandleInfo serves all another incoming messages (Pid ! message)
// HandleInfo -> ("noreply", state) - noreply
//
//	("stop", reason) - normal stop
func (dgs *DebugGenServer) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	return gen.ServerStatusOK
}

// Terminate called when process died
func (dgs *DebugGenServer) Terminate(process *gen.ServerProcess, reason string) {
}
