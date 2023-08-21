package cmd

import (
	"fmt"
	"gamer/common"
	"gamer/config"
	"gamer/log"

	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
)

type TransGen struct {
	genServerName  string
	gateNodeName   string
	transGenServer *TransGenServer
}

func (t *TransGen) Call(cmd ...string) (etf.Term, error) {
	log.Logger.Infof("call node -> %v,%v, cmd: %v", t.genServerName, t.gateNodeName, cmd)
	if len(cmd) == 1 {
		return t.transGenServer.process.Call(gen.ProcessID{Name: t.genServerName, Node: t.gateNodeName}, cmd[0])
		//return transGenServer.process.Call(gen.ProcessID{Name: genServerName, Node: gateNodeName}, etf.Atom(cmd[0]))
	} else {
		return t.transGenServer.process.Call(gen.ProcessID{Name: t.genServerName, Node: t.gateNodeName}, cmd)
	}
}

func (t *TransGen) Register() (etf.Term, error) {
	msg := &common.TransMessage{
		CMD:  common.Register,
		From: config.ServerCfg.Node,
	}
	log.Logger.Infof("call node -> %v,%v, cmd: %v", t.genServerName, t.gateNodeName, msg)

	return t.transGenServer.process.Call(gen.ProcessID{Name: t.genServerName, Node: t.gateNodeName}, msg)

}

func (t *TransGen) Ping() (bool, string) {
	server, err := t.Call("ping")
	if err != nil {
		fmt.Println(err)
		return false, ""
	}
	return true, fmt.Sprint(server)

}

func NewSpawnTrans(node node.Node, gSName, gNName string) (node.Node, gen.Process, *TransGen) {
	t := new(TransGen)
	t.genServerName = gSName
	t.gateNodeName = gNName
	t.transGenServer = &TransGenServer{}
	// Spawn supervisor process
	process, _ := node.Spawn("trans_gen", gen.ProcessOptions{}, t.transGenServer)

	return node, process, t
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
	return gen.ServerStatusOK
}

// HandleCall serves incoming messages sending via gen_server:call
// HandleCall -> ("reply", message, state) - reply
//
//			 ("noreply", _, state) - noreply
//	         ("stop", reason, _) - normal stop
func (dgs *TransGenServer) HandleCall(process *gen.ServerProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	return etf.Term(""), gen.ServerStatusOK
}

// HandleInfo serves all another incoming messages (Pid ! message)
// HandleInfo -> ("noreply", state) - noreply
//
//	("stop", reason) - normal stop
func (dgs *TransGenServer) HandleInfo(process *gen.ServerProcess, message etf.Term) gen.ServerStatus {
	return gen.ServerStatusOK
}

// Terminate called when process died
func (dgs *TransGenServer) Terminate(process *gen.ServerProcess, reason string) {
}
