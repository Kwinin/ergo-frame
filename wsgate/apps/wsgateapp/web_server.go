package wsgateapp

import (
	"crypto/tls"
	"encoding/json"
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/lib"
	"github.com/gorilla/websocket"
	"net/http"
	"wsgate/apps/wsgateapp/state"
	"wsgate/common"
	"wsgate/config"
	"wsgate/log"
)

func createWebActor(gbVar common.GbVar) gen.ServerBehavior {
	return &webServer{GbVar: common.GbVar{
		NodeName: gbVar.NodeName,
		Cfg:      gbVar.Cfg,
		DB:       gbVar.DB,
	}}
}

type webServer struct {
	gen.Web
	common.GbVar
	process *gen.WebProcess
}

func (web *webServer) InitWeb(process *gen.WebProcess, args ...etf.Term) (gen.WebOptions, error) {
	var options gen.WebOptions

	options.Port = uint16(config.Cfg.Web.Port)
	options.Host = config.Cfg.Web.Host
	proto := "http"
	if config.Cfg.Web.Enable {
		cert, err := lib.GenerateSelfSignedCert("gen.Web demo")
		if err != nil {
			return options, err
		}
		options.TLS = &tls.Config{
			Certificates: []tls.Certificate{cert},
		}
		proto = "https"
	}

	mux := http.NewServeMux()

	web.process = process

	webRoot := process.StartWebHandler(&rootHandler{}, gen.WebHandlerOptions{})
	mux.Handle("/", webRoot)
	mux.HandleFunc("/ws", web.handleWebSocketConnection)
	options.Handler = mux

	log.Logger.Infof("Start Web server on %s://%s:%d/\n", proto, options.Host, options.Port)

	return options, nil
}

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func (web *webServer) handleWebSocketConnection(writer http.ResponseWriter, r *http.Request) {
	// Upgrade HTTP request to WebSocket connection
	conn, err := upgrader.Upgrade(writer, r, nil)
	if err != nil {
		log.Logger.Error("Error upgrading connection:", err)
		return
	}
	defer conn.Close()
	for {
		// Read message from the client
		_, message, err := conn.ReadMessage()
		if err != nil {
			log.Logger.Error("Error reading message:", err)
			break
		}

		// Print the received message
		log.Logger.Infof("Received message: %s\n", message)

		web.login(message)

		// Send a response back to the client
		response := "This is the server response."
		err = conn.WriteMessage(websocket.TextMessage, []byte(response))
		if err != nil {
			fmt.Println("Error writing message:", err)
			break
		}
	}
}

type Message struct {
	Account  string `json:"account"`
	Password string `json:"password"`
	Data     string `json:"data"`
}

func (web *webServer) login(message []byte) {
	msg := &Message{}
	if err := json.Unmarshal(message, msg); err != nil {
		log.Logger.Error("消息格式错误")
	}

	opts := gen.RemoteSpawnOptions{
		Name: fmt.Sprintf("player_remote_%s", msg.Account),
	}

	gotPid, err := web.process.RemoteSpawn("Gamer@localhost", "player_remote", opts, msg.Account, msg.Data)
	if err != nil {
		fmt.Println(222)
		log.Logger.Error(err)
	}
	log.Logger.Infof("OK selfName: %s, selfId %s, returnId %d,%s", web.process.Name(), web.process.Self(), gotPid.ID, gotPid.Node)
	log.Logger.Infof("msg %+v", msg)
	sta := state.NewStateModel(msg.Account)
	//store, err := sta.GetAllState(web.DB)
	//if err != nil {
	//	log.Logger.Error(err)
	//}
	//if store.PlayerId != msg.Account {
	//	log.Logger.Infof("login failed")
	//	return
	//}
	//log.Logger.Infof("kwinin %+v", store)
	sta.Pid = gotPid.String()
	sta.PlayerId = msg.Account
	sta.Status = 1
	sta.AddState(web.DB)

	//web.DB.Set("kwinin", string(message))
}
