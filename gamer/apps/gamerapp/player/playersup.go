package player

import (
	"gamer/common"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func CreatePlayerSup(gbVar common.GbVar) gen.SupervisorBehavior {
	return &PlayerSup{GbVar: common.GbVar{
		NodeName: gbVar.NodeName,
		Cfg:      gbVar.Cfg,
		DB:       gbVar.DB,
	}}
}

type PlayerSup struct {
	gen.Supervisor
	common.GbVar
}

func (sup *PlayerSup) Init(args ...etf.Term) (gen.SupervisorSpec, error) {
	spec := gen.SupervisorSpec{
		Name: "playersup",
		Children: []gen.SupervisorChildSpec{
			gen.SupervisorChildSpec{
				Name: "playeractor",
				Child: createPlayerActor(common.GbVar{
					NodeName: sup.NodeName,
					Cfg:      sup.Cfg,
					DB:       sup.DB,
				}),
			},
		},
		Strategy: gen.SupervisorStrategy{
			Type:      gen.SupervisorStrategyOneForOne,
			Intensity: 2, // How big bursts of restarts you want to tolerate.
			Period:    5, // In seconds.
			Restart:   gen.SupervisorStrategyRestartTransient,
		},
	}
	return spec, nil
}
