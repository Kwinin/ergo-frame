package cmd

import (
	"fmt"
	"wsgate/common"
	"wsgate/config"
	"wsgate/log"

	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
)

type TransGen struct {
	toServerName   string
	toNodeName     string
	transGenServer *TransGenServer
	fromServerName string
}

func NewSpawnTrans(node node.Node, toGSName, toGNName string) (node.Node, gen.Process, *TransGen) {
	t := new(TransGen)
	t.toServerName = toGSName
	t.toNodeName = toGNName
	t.transGenServer = &TransGenServer{}
	t.fromServerName = fmt.Sprintf("%s_%d_trans_gen", config.ServerCfg.ServerRole, config.ServerCfg.ServerID)
	// Spawn supervisor process
	process, _ := node.Spawn(t.fromServerName, gen.ProcessOptions{}, t.transGenServer)
	return node, process, t
}

func (t *TransGen) Call(cmd ...string) (etf.Term, error) {
	log.Logger.Infof("call node -> %v,%v, cmd: %v", t.toServerName, t.toNodeName, cmd)
	if len(cmd) == 1 {
		return t.transGenServer.process.Call(gen.ProcessID{Name: t.toServerName, Node: t.toNodeName}, cmd[0])
		//return transGenServer.process.Call(gen.ProcessID{Name: genServerName, Node: gateNodeName}, etf.Atom(cmd[0]))
	} else {
		return t.transGenServer.process.Call(gen.ProcessID{Name: t.toServerName, Node: t.toNodeName}, cmd)
	}
}

func (t *TransGen) Register() (etf.Term, error) {
	msg := &common.TransMessage{
		CMD:           common.Register,
		FromNode:      config.ServerCfg.Node,
		FromGenServer: t.fromServerName,
	}
	log.Logger.Infof("call to node -> %v, %v, cmd: %+v", t.toServerName, t.toNodeName, *msg)

	return t.transGenServer.process.Call(gen.ProcessID{Name: t.toServerName, Node: t.toNodeName}, msg)

}

func (t *TransGen) Ping() (bool, string) {
	server, err := t.Call("ping")
	if err != nil {
		fmt.Println(err)
		return false, ""
	}
	return true, fmt.Sprint(server)

}

func (t *TransGen) NodeRegisterToMaster() (string, error) {
	res, err := t.Register()
	//err.Error() == "no route to node"
	r := res.(string)
	if err != nil || r == "failed" {
		failedStr := fmt.Sprintf("connect %s failed", config.Cfg.MasterAddr)
		//t.transGenServer.process.Exit(failedStr)
		return failedStr, err
	}
	return res.(string), nil
}

func (t *TransGen) Exit() {
	failedStr := fmt.Sprintf("connect %s failed", config.Cfg.MasterAddr)
	t.transGenServer.process.Exit(failedStr)
}

// GenServer implementation structure
type TransGenServer struct {
	gen.Server
	process *gen.ServerProcess
}

// Init initializes process state using arbitrary arguments
// Init(...) -> state
func (dgs *TransGenServer) Init(process *gen.ServerProcess, args ...etf.Term) error {
	dgs.process = process
	return nil
}

// HandleCast serves incoming messages sending via gen_server:cast
// HandleCast -> ("noreply", state) - noreply
//
//	("stop", reason) - stop with reason
func (dgs *TransGenServer) HandleCast(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleCast: %#v \n", message)

	return gen.ServerStatusOK
}

// HandleCall serves incoming messages sending via gen_server:call
// HandleCall -> ("reply", message, state) - reply
//
//			 ("noreply", _, state) - noreply
//	         ("stop", reason, _) - normal stop
func (dgs *TransGenServer) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	log.Logger.Infof("HandleCall: %#v \n", message)
	return etf.Term(""), gen.ServerStatusOK
}

// HandleInfo serves all another incoming messages (Pid ! message)
// HandleInfo -> ("noreply", state) - noreply
//
//	("stop", reason) - normal stop
func (dgs *TransGenServer) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleInfo: %#v \n", message)

	return gen.ServerStatusOK
}

// Terminate called when process died
func (dgs *TransGenServer) Terminate(process *gen.ServerProcess, reason string) {
	log.Logger.Infof("Terminate: %#v \n", reason)
	process.Exit("")

}
