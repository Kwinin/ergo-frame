package main

import (
	"flag"
	"github.com/ergo-services/ergo"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/node"
	"wsgate/apps/wsgateapp"
	"wsgate/apps/wsgateapp/db"
	"wsgate/common"
	"wsgate/config"
	"wsgate/log"
)

var (
	OptionGamerNodeName  string
	OptionWsGateNodeName string
	OptionMasterNodeName string
	OptionNodeCookie     string
)

func init() {
	// generate random value for cookie
	//buff := make([]byte, 12)
	//rand.Read(buff)
	//randomCookie := hex.EncodeToString(buff)

	flag.StringVar(&OptionGamerNodeName, "gamer_name", "Gamer@localhost", "node gamer_name")
	flag.StringVar(&OptionWsGateNodeName, "wsgate_name", "WsGate@localhost", "node wsgate_name")
	flag.StringVar(&OptionMasterNodeName, "master_name", "Master@localhost", "node master_name")
	flag.StringVar(&OptionNodeCookie, "cookie", "cookie123", "a secret cookie for interaction within the cluster")

}

func main() {
	log.InitLogger()

	configPath := "./conf"
	err := config.InitConfig(configPath)
	if err != nil {
		log.Logger.Error(err)
	}

	db, err := db.NewDBClient(config.Cfg.SSDB.Host, config.Cfg.SSDB.Port)
	if err != nil {
		log.Logger.Errorf("%+v", err)
	}

	gbVar := common.GbVar{
		NodeName: OptionWsGateNodeName,
		Cfg:      config.Cfg,
		DB:       db,
	}

	var options node.Options

	flag.Parse()

	// Create applications that must be started
	apps := []gen.ApplicationBehavior{
		wsgateapp.CreateWsGateApp(gbVar),
	}

	listener := node.Listener{
		Listen: 25001,
	}
	options = node.Options{
		Listeners: []node.Listener{listener},
		//Registrar: dist.CreateRegistrarWithLocalEPMD("", 24999), EPMD默认端口4369
	}

	options.Applications = apps
	options.Proxy.Accept = true
	options.Proxy.Transit = true

	// Starting node
	WsGateNode, err := ergo.StartNode(OptionWsGateNodeName, OptionNodeCookie, options)
	if err != nil {
		panic(err)
	}
	log.Logger.Infof("Node %q is started\n", WsGateNode.Name())

	WsGateNode.ProvideRemoteSpawn("wsgate_remote", &wsgateapp.WsGateActor{})

	//opts := gen.RemoteSpawnOptions{
	//	Name: "player_remote",
	//}
	//
	//process, _ := WsGateNode.Spawn("gs1", gen.ProcessOptions{}, &wsgateapp.WsGateActor{}, nil)
	//
	//gotPid, err := process.RemoteSpawn(OptionGamerNodeName, "player_remote", opts, 4, 6, 8)
	//if err != nil {
	//	log.Logger.Error(err)
	//}
	//log.Logger.Infof("OK selfName: %s, selfId %s, returnId %d,%s", process.Name(), process.Self(), gotPid.ID, gotPid.Node)

	//fmt.Println(3434, config.Cfg.Tcp.Host)
	//hostPort := net.JoinHostPort(config.Cfg.Tcp.Host, strconv.Itoa(config.Cfg.Tcp.Port))
	//dialer := net.Dialer{}
	//
	//var connection net.Conn
	//
	//connection, err = dialer.Dial("tcp", hostPort)
	//
	//if err != nil {
	//	return
	//}
	//
	//defer connection.Close()
	//
	//for i := 0; i < 5; i++ {
	//	str := lib.RandomString(16)
	//
	//	fmt.Printf("send string %q to %q\n", str, connection.RemoteAddr().String())
	//	connection.Write([]byte(str))
	//	time.Sleep(time.Second)
	//}

	WsGateNode.Wait()
}
