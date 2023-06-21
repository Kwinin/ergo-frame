package main

import (
	"crypto/rand"
	"encoding/hex"
	"flag"
	"fmt"

	"gamer/apps/gamerapp"

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

	flag.StringVar(&OptionNodeName, "name", "Gamer@localhost", "node name")
	flag.StringVar(&OptionNodeCookie, "cookie", randomCookie, "a secret cookie for interaction within the cluster")

}

func main() {
	var options node.Options

	flag.Parse()

	// Create applications that must be started
	apps := []gen.ApplicationBehavior{
		gamerapp.CreateMyApp(),
	}
	options.Applications = apps

	// Starting node
	GamerNode, err := ergo.StartNode(OptionNodeName, OptionNodeCookie, options)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Node %q is started\n", GamerNode.Name())

	GamerNode.Wait()
}
