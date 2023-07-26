package player

import (
	"gamer/apps/gamerapp/lib"
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
	log.Logger.Infof("Init Player process: %s with name %q and args %v \n", process.Self(), process.Name(), args)
	if process.Name() == "player_remote" {
		role := lib.RoleLib{}
		OnLogin()
		role.LaunchRolePid(process, lib.RoleTag{Tag: "player", RoleId: 3434}, &Server{GbVar: common.GbVar{
			NodeName: s.NodeName,
			Cfg:      s.Cfg,
			DB:       s.DB}}, 222, 343)
	}
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
	log.Logger.Infof("HandleCall: %#v \n", message)
	return nil, gen.ServerStatusOK
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
