package main

import (
	"flag"
	"fmt"
	"wsgate/apps/wsgateapp"
	"wsgate/lib"

	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
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

	flag.StringVar(&OptionNodeName, "name", "WsGate@localhost", "node name")
	flag.StringVar(&OptionNodeCookie, "cookie", "cookie123", "a secret cookie for interaction within the cluster")

}

func main() {
	var options node.Options

	flag.Parse()

	// Create applications that must be started
	apps := []gen.ApplicationBehavior{
		wsgateapp.CreateWsGateApp(),
	}
	options.Applications = apps
	options.Proxy.Accept = true
	options.Proxy.Transit = true

	// Starting node
	WsGateNode, err := ergo.StartNode(OptionNodeName, OptionNodeCookie, options)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Node %q is started\n", WsGateNode.Name())

	WsGateNode.ProvideRemoteSpawn("remote", &lib.HandshakeGenServer{})

	WsGateNode.Wait()
}
