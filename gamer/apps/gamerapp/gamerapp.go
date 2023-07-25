package gamerapp

import (
	"gamer/apps/gamerapp/db"
	"gamer/common"
	"gamer/config"
	"gamer/log"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

type GamerApp struct {
	gen.Application
	common.GbVar
}

func CreateGamerApp() gen.ApplicationBehavior {
	return &GamerApp{}
}

func (app *GamerApp) Load(args ...etf.Term) (gen.ApplicationSpec, error) {

	app.initApp()
	return gen.ApplicationSpec{
		Name:        "gamerapp",
		Description: "description of this application",
		Version:     "v.1.0",
		Children: []gen.ApplicationChildSpec{
			gen.ApplicationChildSpec{
				Name:  "gamersup",
				Child: createGamerSup(),
			},
		},
	}, nil
}

func (app *GamerApp) Start(process gen.Process, args ...etf.Term) {
	log.Logger.Infof("Application GamerApp started with Pid %s\n", process.Self())
}

func (app *GamerApp) initApp() {
	app.setGbConfig()
	app.setGbDb()
}

func (app *GamerApp) setGbConfig() {
	app.Cfg = config.Cfg

}

func (app *GamerApp) setGbDb() {
	db, err := db.NewDBClient(app.Cfg.SSDB.Host, app.Cfg.SSDB.Port)
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}
	log.Logger.Info("connect ssdb successful")
	app.DB = db

}
