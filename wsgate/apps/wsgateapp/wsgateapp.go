package wsgateapp

import (
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func CreateWsGateApp() gen.ApplicationBehavior {
	return &WsGateApp{}
}

type WsGateApp struct {
	gen.Application
}

func (app *WsGateApp) Load(args ...etf.Term) (gen.ApplicationSpec, error) {
	return gen.ApplicationSpec{
		Name:        "wsgateapp",
		Description: "description of this application",
		Version:     "v.1.0",
		Children: []gen.ApplicationChildSpec{
			gen.ApplicationChildSpec{
				Name:  "wsgatesup",
				Child: createWsGateSup(),
			},
		},
	}, nil
}

func (app *WsGateApp) Start(process gen.Process, args ...etf.Term) {
	fmt.Printf("Application WsGateApp started with Pid %s\n", process.Self())
}
