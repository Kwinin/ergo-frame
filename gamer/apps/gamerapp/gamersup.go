package gamerapp

import (
	"gamer/apps/gamerapp/player"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func createGamerSup() gen.SupervisorBehavior {
	return &GamerSup{}
}

type GamerSup struct {
	gen.Supervisor
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
				Name:  "playersup",
				Child: player.CreatePlayerSup(),
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
