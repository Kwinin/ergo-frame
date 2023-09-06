package server

import (
	"fmt"
	"github.com/facebookgo/pidfile"
	"github.com/sirupsen/logrus"
	"master/apps/masterapp/node"
	"master/common"
	"master/config"
	"master/db"
	"master/log"
	"master/nodesManage"
	"os"
)

var MainServerInfo *mainServer

type mainServer struct {
	command chan string
	db      *db.DBClient
}

func (m *mainServer) OpenConn() {
}

func (m *mainServer) CloseConn() {
}

func (m *mainServer) Start() {
	//启动网络
	nodesManage.Start(m.command, m.db)

	//alNode := nodes.GetNode(common.MasterGenServer, config.ServerCfg.ServerID)

}

func (m *mainServer) Close() {
	for _, node := range nodesManage.GetNodes() {
		for _, process := range node.ProcessList() {
			process.Exit("server stop")
		}
		node.Stop()
	}
}

func StartServer() {

	filename := fmt.Sprintf("./tmp/pid_%v_%v", config.ServerCfg.ServerName, config.ServerCfg.ServerID)
	pidfile.SetPidfilePath(filename)
	if i, _ := pidfile.Read(); i != 0 {
		log.Logger.Warnf("服务已启动请检查或清除 进程id [%v] pidfile: [%v]  ", i, filename)
		return
	}

	db, err := db.InfDb.NewDBClient(&db.SctSSdb{})
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}

	MainServerInfo = &mainServer{command: make(chan string), db: db}
	MainServerInfo.Start()

	defer db.Close()
	defer CloseServer()
	defer MainServerInfo.Close()
	StartSuccess()
	for {
		select {
		case command := <-MainServerInfo.command:
			switch command {
			case common.StartSuccess:
				pid := StartSuccess()
				log.Logger.Infof("====================== Start Game Server pid:[%v] Success =========================", pid)
			case common.Shutdown:
				nd := node.NewNodesModel()
				err := nd.ClearNodes(db)
				if err != nil {
					log.Logger.Errorf("db op %v", err)
				}
				MainServerInfo.CloseConn()
				log.Logger.Infof("Shut down the game server")
				return
			case "OpenConn":
				MainServerInfo.OpenConn()
			case "CloseConn":
				MainServerInfo.CloseConn()
			default:
				logrus.Warn("command:", command)
			}
		}
	}

}
func StartSuccess() int {
	pidfile.Write()
	log.Logger.Infof("pidfile :%v", pidfile.GetPidfilePath())
	i, _ := pidfile.Read()
	return i
}

func CloseServer() {
	log.Logger.Info("delete pidfile: ", pidfile.GetPidfilePath())
	os.Remove(pidfile.GetPidfilePath())
}
