package player

import (
	"gamer/log"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

var logger = log.InfLog.GetLogger(log.Logrus{})

func CreatePlayerSup() gen.SupervisorBehavior {
	return &PlayerSup{}
}

type PlayerSup struct {
	gen.Supervisor
}

func (sup *PlayerSup) Init(args ...etf.Term) (gen.SupervisorSpec, error) {
	spec := gen.SupervisorSpec{
		Name: "playersup",
		Children: []gen.SupervisorChildSpec{
			gen.SupervisorChildSpec{
				Name:  "playeractor",
				Child: createPlayerActor(),
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
