package masterapp

import (
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"master/config"
	"master/db"
)

func createMasterSup() gen.SupervisorBehavior {
	return &MasterSup{}
}

type MasterSup struct {
	gen.Supervisor
	CmdChan chan string
	DB      *db.DBClient
}

func (sup *MasterSup) Init(args ...etf.Term) (gen.SupervisorSpec, error) {
	sup.CmdChan = args[0].(chan string)
	sup.DB = args[1].(*db.DBClient)
	spec := gen.SupervisorSpec{
		Name: fmt.Sprintf("%s_%d_sup", config.ServerCfg.ServerRole, config.ServerCfg.ServerID),
		Children: []gen.SupervisorChildSpec{
			gen.SupervisorChildSpec{
				Name:  fmt.Sprintf("%s_%d_actor", config.ServerCfg.ServerRole, config.ServerCfg.ServerID),
				Child: createMasterActor(),
				Args: []etf.Term{
					sup.CmdChan,
					sup.DB,
				},
			},
		},
		Strategy: gen.SupervisorStrategy{
			Type:      gen.SupervisorStrategyRestForOne,
			Intensity: 2, // How big bursts of restarts you want to tolerate.
			Period:    5, // In seconds.
			Restart:   gen.SupervisorStrategyRestartTransient,
		},
	}
	return spec, nil
}
