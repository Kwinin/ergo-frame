package gamerapp

import (
	"gamer/common"
	"gamer/log"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

type GamerApp struct {
	gen.Application
	common.GbVar
}

func CreateGamerApp(gbVar common.GbVar) gen.ApplicationBehavior {
	return &GamerApp{GbVar: common.GbVar{
		NodeName: gbVar.NodeName,
		Cfg:      gbVar.Cfg,
		DB:       gbVar.DB,
	}}
}

func (app *GamerApp) Load(args ...etf.Term) (gen.ApplicationSpec, error) {

	app.initApp()
	return gen.ApplicationSpec{
		Name:        "gamerapp",
		Description: "description of this application",
		Version:     "v.1.0",
		Children: []gen.ApplicationChildSpec{
			gen.ApplicationChildSpec{
				Name: "gamersup",
				Child: createGamerSup(common.GbVar{
					NodeName: app.NodeName,
					Cfg:      app.Cfg,
					DB:       app.DB,
				}),
			},
		},
	}, nil
}

func (app *GamerApp) Start(process gen.Process, args ...etf.Term) {
	log.Logger.Infof("Application GamerApp started with Pid %s\n", process.Self())
}

func (app *GamerApp) initApp() {

}
