package main

import (
	"crypto/rand"
	"encoding/hex"
	"flag"
	"fmt"

	"master/apps/masterapp"

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
	buff := make([]byte, 12)
	rand.Read(buff)
	randomCookie := hex.EncodeToString(buff)

	flag.StringVar(&OptionNodeName, "name", "Master@localhost", "node name")
	flag.StringVar(&OptionNodeCookie, "cookie", randomCookie, "a secret cookie for interaction within the cluster")

}

func main() {
	var options node.Options

	flag.Parse()

	// Create applications that must be started
	apps := []gen.ApplicationBehavior{
		masterapp.CreateMasterApp(),
	}
	options.Applications = apps

	// Starting node
	MasterNode, err := ergo.StartNode(OptionNodeName, OptionNodeCookie, options)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Node %q is started\n", MasterNode.Name())

	MasterNode.Wait()
}
