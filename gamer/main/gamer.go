package main

import (
	"flag"
	"gamer/apps/gamerapp"
	"gamer/apps/gamerapp/player"
	"gamer/cmd"
	"gamer/common"
	"gamer/config"
	"gamer/db"
	"gamer/log"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
	"time"
)

func main() {
	log.InitLogger()

	err := config.InitConfig(config.ServerCfg.CfgPath)
	if err != nil {
		log.Logger.Error(err)
	}
	db, err := db.InfDb.NewDBClient(&db.SctSSdb{})
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}

	gbVar := common.GbVar{
		NodeName: config.ServerCfg.Node.Addr,
		Cfg:      config.Cfg,
		DB:       db,
	}

	var options node.Options

	flag.Parse()

	// Create applications that must be started
	apps := []gen.ApplicationBehavior{
		gamerapp.CreateGamerApp(gbVar),
	}
	options.Applications = apps
	options.Proxy.Accept = true
	options.Proxy.Transit = true

	// Starting node
	GamerNode, err := ergo.StartNode(config.ServerCfg.Node.Addr, config.ServerCfg.Cookie, options)
	if err != nil {
		panic(err)
	}
	log.Logger.Infof("Node %q is started\n", GamerNode.Name())

	err = GamerNode.ProvideRemoteSpawn("gamer_remote", &gamerapp.GamerActor{})
	if err != nil {
		log.Logger.Error(err)
	}
	sendChan := make(chan []byte, 1)

	err = GamerNode.ProvideRemoteSpawn("player_remote", &player.PlayerGenServer{
		GbVar:    gbVar,
		SendChan: sendChan,
	})

	p := GamerNode.ProcessByName("playeractor")
	if err != nil {
		log.Logger.Error(err)

	}
	go func() {
		_, _, Tg := cmd.NewSpawnTrans(GamerNode, "master_1_actor", config.Cfg.MasterAddr)

		connect := false
		for {
			err = GamerNode.Connect(config.Cfg.MasterAddr)
			if err != nil {
				// 找不到 master 节点
				connect = false
				log.Logger.Infof("disconnect  %s reason: %v", config.Cfg.MasterAddr, err)

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

	for {
		select {
		case buf := <-sendChan:
			err := p.Send(gen.ProcessID{Name: "web", Node: "WsGate@localhost"}, etf.Term(etf.Tuple{etf.Atom("$gen_cast"), buf}))
			if err != nil {
				log.Logger.Error(err)
			}
		}
	}

}
