package nodesManage

import (
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
	"master/apps/masterapp"
	"master/config"
	"master/db"
	"master/log"
)

func StartMasterNode(cmd chan string, db *db.DBClient) (node.Node, error) {
	var options node.Options

	lis := node.Listener{
		ListenBegin: uint16(config.ServerCfg.ListenBegin),
		ListenEnd:   uint16(config.ServerCfg.ListenEnd),
	}

	// Create applications that must be started
	apps := []gen.ApplicationBehavior{
		masterapp.CreateMasterApp(cmd, db),
	}
	options.Applications = apps
	options.Proxy.Flags = node.DefaultProxyFlags()
	options.Proxy.Flags.EnableRemoteSpawn = false
	options.Listeners = []node.Listener{lis}

	// Starting node
	MasterNode, err := ergo.StartNode(config.ServerCfg.Node.Addr, config.ServerCfg.Cookie, options)
	if err != nil {
		return nil, err
	}
	log.Logger.Infof("Node %q is started\n", MasterNode.Name())

	// todo: proxy net
	//route := node.ProxyRoute{
	//	Name:  config.Cfg.NodeList.Gamer[0].Addr,
	//	Proxy: config.Cfg.NodeList.WsGate[0].Addr,
	//}
	//
	//MasterNode.AddProxyRoute(route)

	//if err := MasterNode.Connect(config.Cfg.Nodes.WsGate[0].Addr); err != nil {
	//	fmt.Println(111, err)
	//}
	return MasterNode, nil
}
