package main

import (
	"flag"
	"gamer/apps/gamerapp"
	"gamer/apps/gamerapp/db"
	"gamer/apps/gamerapp/player"
	"gamer/cmd"
	"gamer/common"
	"gamer/config"
	"gamer/log"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
)

func main() {
	log.InitLogger()

	err := config.InitConfig(config.ServerCfg.CfgPath)
	if err != nil {
		log.Logger.Error(err)
	}

	db, err := db.NewDBClient(config.ServerCfg.SSDB.Host, config.ServerCfg.SSDB.Port)
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}

	gbVar := common.GbVar{
		NodeName: config.ServerCfg.Node.Addr,
		Cfg:      config.Cfg,
		DB:       db,
	}
	//go func() {
	//	for {
	//
	//		fmt.Printf("kwinin  %v %v\n", config.Cfg.DATABASE.Url, config.Cfg.DATABASE.Username)
	//		time.Sleep(5 * time.Second)
	//	}
	//}()

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
	//
	GamerNode.ProvideRemoteSpawn("gamer_remote", &gamerapp.GamerActor{})
	GamerNode.ProvideRemoteSpawn("player_remote", &player.Actor{GbVar: gbVar})

	log.Logger.Info(GamerNode.Nodes())

	_, _, Tg := cmd.NewSpawnTrans(GamerNode, "master_1_actor", "Master@localhost")

	Tg.Register()

	GamerNode.Wait()
}
