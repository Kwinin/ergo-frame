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

	opts := gen.RemoteSpawnOptions{
		Name:     "remote",
		Function: "Hello",
	}

	gotPid, err := process.RemoteSpawn("WsGate@localhost", "remote", opts, 1, 2, 3)
	if err != nil {
		fmt.Println(134, err)
	}
	fmt.Println("OK", process.Name(), process.Self(), gotPid)

}
