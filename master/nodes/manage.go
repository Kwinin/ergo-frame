package nodes

import (
	"github.com/ergo-services/ergo/node"
	"master/common"
	"master/config"
	"master/log"
	"sync"
)

// ergo.Node 节点管理
var (
	nodesMap  sync.Map
	remoteMap sync.Map //远程连接节点
)

func Start(command chan string) {
	serverNode, err := StartMasterNode(command)
	if err != nil {
		panic(err)
	}
	log.Logger.Infof("severnode Name %s", serverNode.Name())
	nodesMap.Store(serverNode.Name(), serverNode)
}

func GetNode(name common.GenServerName, serverId int32) node.Node {
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
