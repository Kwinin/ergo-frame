package nodesManage

import (
	"github.com/ergo-services/ergo/node"
	"master/config"
	"master/db"
	"master/log"
	"sync"
)

// ergo.Node 节点管理
var (
	nodesMap      sync.Map
	remoteNodeMap sync.Map
)

func Start(command chan string, db *db.DBClient) {
	serverNode, err := StartMasterNode(command, db)
	if err != nil {
		panic(err)
	}
	log.Logger.Infof("severnode Name %s", serverNode.Name())
	nodesMap.Store(serverNode.Name(), serverNode)
}

func GetNode(name string, serverId int32) node.Node {
	nodeC, err := config.GetNodeInfo(name, serverId)
	if err != nil {
		log.Logger.Error("GetNode ", err)
		return nil
	}
	if v, ok := nodesMap.Load(nodeC.Addr); ok {
		return v.(node.Node)
	}
	return nil
}

func GetNodes() map[string]node.Node {
	nodemap := map[string]node.Node{}
	nodesMap.Range(func(key, value interface{}) bool {
		nodemap[key.(string)] = value.(node.Node)
		return true
	})
	return nodemap
}

type RemoteMapStc struct {
	Addr string
	Used int8
}

func SetRemoteNode(conf config.NodeConf) {
	data := &RemoteMapStc{
		Addr: conf.Addr,
		Used: 1,
	}
	remoteNodeMap.Store(conf.Name, data)
}
