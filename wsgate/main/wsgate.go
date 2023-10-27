package main

import (
	"flag"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
	"os"
	"time"
	"wsgate/apps/wsgateapp"
	"wsgate/cmd"
	"wsgate/common"
	"wsgate/config"
	"wsgate/db"
	"wsgate/log"
)

func main() {
	log.InitLogger()

	err := config.InitConfig(os.Getenv("ERGO_ENV_CONF"))
	if err != nil {
		log.Logger.Error(err)
	}

	db, err := db.InfDb.NewDBClient(&db.SctSSdb{})
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}

	gbVar := common.GbVar{
		NodeName: config.ServerCfg.Node.Addr,
		Cfg:      config.ServerCfg,
		DB:       db,
	}

	var options node.Options

	flag.Parse()

	// Create applications that must be started
	apps := []gen.ApplicationBehavior{
		wsgateapp.CreateWsGateApp(gbVar),
	}

	listener := node.Listener{
		ListenBegin: config.ServerCfg.ListenBegin,
		ListenEnd:   config.ServerCfg.ListenEnd,
	}
	options = node.Options{
		Listeners: []node.Listener{listener},
		//Registrar: dist.CreateRegistrarWithLocalEPMD("", 24999), EPMD默认端口4369
	}

	options.Applications = apps
	options.Proxy.Accept = true
	options.Proxy.Transit = true

	// Starting node
	WsGateNode, err := ergo.StartNode(config.ServerCfg.Node.Addr, config.ServerCfg.Cookie, options)
	if err != nil {
		panic(err)
	}
	log.Logger.Infof("Node %q is started\n", WsGateNode.Name())

	go func() {
		_, _, Tg := cmd.NewSpawnTrans(WsGateNode, "master_1_actor", config.ServerCfg.MasterAddr)

		connect := false
		for {
			err = WsGateNode.Connect(config.ServerCfg.MasterAddr)
			if err != nil {
				// 找不到 master 节点
				connect = false
				log.Logger.Infof("disconnect  %s reason: %v", config.ServerCfg.MasterAddr, err)

			} else {
				if connect == false {
					conStr, _ := Tg.NodeRegisterToMaster()
					log.Logger.Info(conStr)
					connect = true
				}
			}
			time.Sleep(5 * time.Second)
		}
	}()
	WsGateNode.Wait()
}
