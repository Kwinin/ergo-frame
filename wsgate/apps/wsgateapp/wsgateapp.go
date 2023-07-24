package wsgateapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/seefan/gossdb"
	"github.com/sirupsen/logrus"
	"wsgate/apps/wsgateapp/db"
	"wsgate/config"
	"wsgate/log"
)

var logger = log.InfLog.GetLogger(log.Logrus{})

//	type InfAppDb interface {
//		NewDBClient() (*db.DBClient, error)
//	}

type GbVar struct {
	name   string
	cfg    config.Conf
	db     *gossdb.Client
	logger *logrus.Logger
}

type WsGateApp struct {
	gen.Application
	GbVar
}

func CreateWsGateApp() gen.ApplicationBehavior {
	return &WsGateApp{}
}

func (app *WsGateApp) Load(args ...etf.Term) (gen.ApplicationSpec, error) {
	return gen.ApplicationSpec{
		Name:        "App",
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
	app.initApp()
	logger.Infof("Application App started with Pid %s\n", process.Self())
}

func (app *WsGateApp) initApp() {
	app.setGbLogger()
	app.setGbConfig()
	app.setGbDb()
	app.startHttp()
}
func (app *WsGateApp) setGbConfig() {
	app.cfg = config.Cfg

}
func (app *WsGateApp) setGbDb() {
	db, err := db.GetDBClient()
	if err != nil {
		logger.Errorf("%+v", err)
	}
	app.logger.Info("连接SSDB数据库成功")
	app.db = db

}
func (app *WsGateApp) startHttp() {

}

func (app *WsGateApp) setGbLogger() {
	app.logger = logger
}
