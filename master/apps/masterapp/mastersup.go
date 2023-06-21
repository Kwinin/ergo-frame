package masterapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func createMasterSup() gen.SupervisorBehavior {
	return &MasterSup{}
}

type MasterSup struct {
	gen.Supervisor
}

func (sup *MasterSup) Init(args ...etf.Term) (gen.SupervisorSpec, error) {
	spec := gen.SupervisorSpec{
		Name: "mastersup",
		Children: []gen.SupervisorChildSpec{
			gen.SupervisorChildSpec{
				Name:  "masteractor",
				Child: createMasterActor(),
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
