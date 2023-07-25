package masterapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/sirupsen/logrus"
	"master/config"
	"master/log"
)

func CreateMasterApp() gen.ApplicationBehavior {
	return &MasterApp{}
}

type GbVar struct {
	name   string
	cfg    config.Conf
	logger *logrus.Logger
}

type MasterApp struct {
	gen.Application
	GbVar
}

func (app *MasterApp) Load(args ...etf.Term) (gen.ApplicationSpec, error) {
	return gen.ApplicationSpec{
		Name:        "masterapp",
		Description: "description of this application",
		Version:     "v.1.0",
		Children: []gen.ApplicationChildSpec{
			gen.ApplicationChildSpec{
				Name:  "mastersup",
				Child: createMasterSup(),
			},
		},
	}, nil
}

func (app *MasterApp) Start(process gen.Process, args ...etf.Term) {
	app.setGbLogger()
	app.logger.Infof("Application MasterApp started with Pid %s\n", process.Self())

}

func (app *MasterApp) setGbLogger() {
	app.logger = log.Logger
}
