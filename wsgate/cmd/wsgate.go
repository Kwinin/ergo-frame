package main

import (
	"flag"
	"fmt"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
	"wsgate/apps/wsgateapp"
)

var (
	OptionGamerNodeName  string
	OptionWsGateNodeName string
	OptionMasterNodeName string
	OptionNodeCookie     string
)

func init() {
	// generate random value for cookie
	//buff := make([]byte, 12)
	//rand.Read(buff)
	//randomCookie := hex.EncodeToString(buff)

	flag.StringVar(&OptionGamerNodeName, "gamer_name", "Gamer@localhost", "node gamer_name")
	flag.StringVar(&OptionWsGateNodeName, "wsgate_name", "WsGate@localhost", "node wsgate_name")
	flag.StringVar(&OptionMasterNodeName, "master_name", "Master@localhost", "node master_name")
	flag.StringVar(&OptionNodeCookie, "cookie", "cookie123", "a secret cookie for interaction within the cluster")

}

func main() {
	var options node.Options

	flag.Parse()

	// Create applications that must be started
	apps := []gen.ApplicationBehavior{
		wsgateapp.CreateWsGateApp(),
	}

	listener := node.Listener{
		Listen: 25001,
	}
	options = node.Options{
		Listeners: []node.Listener{listener},
		//Registrar: dist.CreateRegistrarWithLocalEPMD("", 24999), EPMD默认端口4369
	}

	options.Applications = apps
	options.Proxy.Accept = true
	options.Proxy.Transit = true

	// Starting node
	WsGateNode, err := ergo.StartNode(OptionWsGateNodeName, OptionNodeCookie, options)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Node %q is started\n", WsGateNode.Name())

	WsGateNode.ProvideRemoteSpawn("wsgate_remote", &wsgateapp.WsGateActor{})

	opts := gen.RemoteSpawnOptions{
		Name: "player_remote",
	}

	process, _ := WsGateNode.Spawn("gs1", gen.ProcessOptions{}, &wsgateapp.WsGateActor{}, nil)

	gotPid, err := process.RemoteSpawn(OptionGamerNodeName, "player_remote", opts, 4, 6, 8)
	if err != nil {
		fmt.Println(134, err)
	}
	fmt.Println("OK", process.Name(), process.Self(), gotPid)

	WsGateNode.Wait()
}
