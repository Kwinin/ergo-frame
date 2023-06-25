package masterapp

import (
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

func CreateMasterApp() gen.ApplicationBehavior {
	return &MasterApp{}
}

type MasterApp struct {
	gen.Application
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
	fmt.Printf("Application MasterApp started with Pid %s\n", process.Self())

}
