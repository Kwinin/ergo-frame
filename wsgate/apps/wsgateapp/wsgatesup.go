package wsgateapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"wsgate/common"
)

func createWsGateSup(gbVar common.GbVar) gen.SupervisorBehavior {
	return &WsGateSup{GbVar: common.GbVar{
		NodeName: gbVar.NodeName,
		Cfg:      gbVar.Cfg,
		DB:       gbVar.DB,
	}}
}

type WsGateSup struct {
	gen.Supervisor
	common.GbVar
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
				Name: "web",
				Child: createWebActor(common.GbVar{
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
