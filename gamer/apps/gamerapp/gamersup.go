package gamerapp

import (
	"gamer/apps/gamerapp/player"
	"gamer/common"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func createGamerSup(gbVar common.GbVar) gen.SupervisorBehavior {
	return &GamerSup{GbVar: common.GbVar{
		NodeName: gbVar.NodeName,
		Cfg:      gbVar.Cfg,
		DB:       gbVar.DB,
	}}
}

type GamerSup struct {
	gen.Supervisor
	common.GbVar
}

func (sup *GamerSup) Init(args ...etf.Term) (gen.SupervisorSpec, error) {
	spec := gen.SupervisorSpec{
		Name: "gamersup",
		Children: []gen.SupervisorChildSpec{
			gen.SupervisorChildSpec{
				Name:  "gameractor",
				Child: createGamerActor(),
			},
			gen.SupervisorChildSpec{
				Name: "playersup",
				Child: player.CreatePlayerSup(common.GbVar{
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
