package server

import (
	"fmt"
	"github.com/facebookgo/pidfile"
	"github.com/sirupsen/logrus"
	"master/common"
	"master/config"
	"master/log"
	"master/nodes"
	"os"
)

var MainServerInfo *mainServer

type mainServer struct {
	command chan string
}

func (m *mainServer) OpenConn() {
}

func (m *mainServer) CloseConn() {
}

func (m *mainServer) Start() {
	//启动网络
	nodes.Start(m.command)

	//alNode := nodes.GetNode(common.MasterGenServer, config.ServerCfg.ServerID)

}

func (m *mainServer) Close() {
	for _, node := range nodes.GetNodes() {
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
	MainServerInfo = &mainServer{command: make(chan string)}
	MainServerInfo.Start()
	defer CloseServer()
	defer MainServerInfo.Close()
	StartSuccess()
	for {
		select {
		case command := <-MainServerInfo.command:
			switch command {
			case string(common.StartSuccess):
				pid := StartSuccess()
				log.Logger.Infof("====================== Start Game Server pid:[%v] Success =========================", pid)
			case string(common.Shutdown):
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
