package masterapp

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"master/db"
	"master/log"
)

func CreateMasterApp(cmd chan string, db *db.DBClient) gen.ApplicationBehavior {
	return &MasterApp{CmdChan: cmd, DB: db}
}

type MasterApp struct {
	gen.Application
	DB      *db.DBClient
	CmdChan chan string
}

func (app *MasterApp) Load(args ...etf.Term) (gen.ApplicationSpec, error) {
	app.initApp()
	return gen.ApplicationSpec{
		Name:        "masterapp",
		Description: "description of this application",
		Version:     "v.1.0",
		Children: []gen.ApplicationChildSpec{
			gen.ApplicationChildSpec{
				Name:  "mastersup",
				Child: createMasterSup(),
				Args: []etf.Term{
					app.CmdChan,
					app.DB,
				},
			},
		},
	}, nil
}

func (app *MasterApp) Start(process gen.Process, args ...etf.Term) {
	log.Logger.Infof("Application MasterApp started with Pid %s\n", process.Self())

}

func (app *MasterApp) initApp() {

}
