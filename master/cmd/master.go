package main

import (
	"flag"
	"fmt"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
	"master/apps/masterapp"
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
		masterapp.CreateMasterApp(),
	}
	options.Applications = apps
	options.Proxy.Flags = node.DefaultProxyFlags()
	options.Proxy.Flags.EnableRemoteSpawn = false

	// Starting node
	MasterNode, err := ergo.StartNode(OptionMasterNodeName, OptionNodeCookie, options)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Node %q is started\n", MasterNode.Name())

	route := node.ProxyRoute{
		Name:  OptionGamerNodeName,
		Proxy: OptionWsGateNodeName,
	}

	MasterNode.AddProxyRoute(route)

	if err := MasterNode.Connect(OptionWsGateNodeName); err != nil {
		fmt.Println(111, err)
	}

	opts := gen.RemoteSpawnOptions{
		Name: "gamer_remote",
	}

	process, _ := MasterNode.Spawn("gs1", gen.ProcessOptions{}, &masterapp.MasterActor{})

	gotPid, err := process.RemoteSpawn(OptionGamerNodeName, "gamer_remote", opts, 1, 2, 3)
	if err != nil {
		fmt.Println(134, err)
	}
	fmt.Println("OK", process.Name(), process.Self(), gotPid)
	MasterNode.Wait()
}
