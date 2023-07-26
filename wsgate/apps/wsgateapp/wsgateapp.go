package wsgateapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"wsgate/common"
	"wsgate/log"
)

//	type InfAppDb interface {
//		NewDBClient() (*db.DBClient, error)
//	}

type WsGateApp struct {
	gen.Application
	common.GbVar
}

func CreateWsGateApp(gbVar common.GbVar) gen.ApplicationBehavior {
	return &WsGateApp{GbVar: common.GbVar{
		NodeName: gbVar.NodeName,
		Cfg:      gbVar.Cfg,
		DB:       gbVar.DB,
	}}
}

func (app *WsGateApp) Load(args ...etf.Term) (gen.ApplicationSpec, error) {
	app.initApp()

	return gen.ApplicationSpec{
		Name:        "App",
		Description: "description of this application",
		Version:     "v.1.0",
		Children: []gen.ApplicationChildSpec{
			gen.ApplicationChildSpec{
				Name: "wsgatesup",
				Child: createWsGateSup(common.GbVar{
					NodeName: app.NodeName,
					Cfg:      app.Cfg,
					DB:       app.DB,
				}),
			},
		},
	}, nil
}

func (app *WsGateApp) Start(process gen.Process, args ...etf.Term) {

	log.Logger.Infof("Application App started with Pid %s\n", process.Self())
}

func (app *WsGateApp) initApp() {
	app.startHttp()
}

func (app *WsGateApp) startHttp() {

}
