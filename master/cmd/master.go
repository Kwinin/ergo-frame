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
	OptionNodeName   string
	OptionNodeCookie string
)

func init() {
	// generate random value for cookie
	//buff := make([]byte, 12)
	//rand.Read(buff)
	//randomCookie := hex.EncodeToString(buff)

	flag.StringVar(&OptionNodeName, "name", "Master@localhost", "node name")
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
	MasterNode, err := ergo.StartNode(OptionNodeName, OptionNodeCookie, options)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Node %q is started\n", MasterNode.Name())

	route := node.ProxyRoute{
		Name:  "Gamer@localhost",
		Proxy: "WsGate@localhost",
	}

	MasterNode.AddProxyRoute(route)

	if err := MasterNode.Connect("WsGate@localhost"); err != nil {
		fmt.Println(111, err)
	}

	MasterNode.Wait()
}
