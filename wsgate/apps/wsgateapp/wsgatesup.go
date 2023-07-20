package wsgateapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func createWsGateSup() gen.SupervisorBehavior {
	return &WsGateSup{}
}

type WsGateSup struct {
	gen.Supervisor
}

func (sup *WsGateSup) Init(args ...etf.Term) (gen.SupervisorSpec, error) {
	spec := gen.SupervisorSpec{
		Name: "wsgatesup",
		Children: []gen.SupervisorChildSpec{
			gen.SupervisorChildSpec{
				Name:  "wsgateactor",
				Child: createWsGateActor(),
			},
			gen.SupervisorChildSpec{
				Name:  "tcp",
				Child: createTcpActor(),
			},
			gen.SupervisorChildSpec{
				Name:  "web",
				Child: createWebActor(),
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
