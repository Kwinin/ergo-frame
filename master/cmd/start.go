package cmd

import (
	"flag"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
	"github.com/spf13/cobra"
	"master/apps/masterapp"
	"master/config"
	"master/log"
)

var startCmd = &cobra.Command{
	Use:   "start",
	Short: "启动服务",
	Long: `	
	ERGO-FRAME
==========================================
	| START |
==========================================
	-t ---使用文档
	`,
	Run: func(cmd *cobra.Command, args []string) {
		start()
	},
}

func init() {
	rootCmd.AddCommand(startCmd)
	startCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}

func start() {
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
	MasterNode, err := ergo.StartNode(config.ServerCfg.Node.Addr, config.ServerCfg.Cookie, options)
	if err != nil {
		panic(err)
	}
	log.Logger.Infof("Node %q is started\n", MasterNode.Name())

	// todo: proxy net
	route := node.ProxyRoute{
		Name:  config.Cfg.NodeList.Gamer[0].Addr,
		Proxy: config.Cfg.NodeList.WsGate[0].Addr,
	}

	MasterNode.AddProxyRoute(route)

	//if err := MasterNode.Connect(config.Cfg.Nodes.WsGate[0].Addr); err != nil {
	//	fmt.Println(111, err)
	//}

	//opts := gen.RemoteSpawnOptions{
	//	Name: "gamer_remote",
	//}
	//
	//process, _ := MasterNode.Spawn("gs1", gen.ProcessOptions{}, &masterapp.MasterActor{})
	//
	//gotPid, err := process.RemoteSpawn(OptionGamerNodeName, "gamer_remote", opts, 1, 2, 3)
	//if err != nil {
	//	fmt.Println(134, err)
	//}
	//fmt.Println("OK", process.Name(), process.Self(), gotPid)
	MasterNode.Wait()
}
