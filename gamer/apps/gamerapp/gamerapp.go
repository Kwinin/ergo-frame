package gamerapp

import (
	"gamer/log"

	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

var logger = log.InfLog.GetLogger(log.Logrus{})

func CreateMyApp() gen.ApplicationBehavior {
	return &MyApp{}
}

type MyApp struct {
	gen.Application
}

func (app *MyApp) Load(args ...etf.Term) (gen.ApplicationSpec, error) {
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

func (app *MyApp) Start(process gen.Process, args ...etf.Term) {
	logger.Infof("Application GamerApp started with Pid %s\n", process.Self())
}
