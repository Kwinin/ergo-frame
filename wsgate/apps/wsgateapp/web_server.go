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

		msg := &Message{}
		if err := json.Unmarshal(message, msg); err != nil {
			log.Logger.Error("消息格式错误")
		}

		response := "This is the server response."
		switch true {
		case msg.Code == 0:
			// 登录
			response, err = web.login(msg)
			if err != nil {
				log.Logger.Error(err)
			}

		case msg.Code == 1:
			// 注销
			sta := state.NewStateModel(msg.Account)
			store, err := sta.GetAllState(web.DB)
			if err != nil {
				log.Logger.Error(err)
			}
			name := fmt.Sprintf("player_remote_%d", store.PlayerId)

			// todo: rand a gamer node
			res, err := web.process.Call(gen.ProcessID{Name: name, Node: "Gamer@localhost"}, msg)
			if err != nil {
				log.Logger.Infof("callerr %+v", err)
				return
			}
			err = sta.ClearState(web.DB)
			response = fmt.Sprintf("user process: %s: account: %d exited", name, res)
		case msg.Code > 1:
			name := fmt.Sprintf("player_remote_%d", msg.Account)

			// todo: rand a gamer node
			res, err := web.process.Call(gen.ProcessID{Name: name, Node: "Gamer@localhost"}, msg)
			if err != nil {
				log.Logger.Infof("callerr %+v", err)
				return
			}
			response = fmt.Sprintf("successful  %+v", res)

		default:
			response = fmt.Sprintf("unknown code %d", msg.Code)

		}

		// Send a response back to the client

		err = conn.WriteMessage(websocket.TextMessage, []byte(response))
		if err != nil {
			fmt.Println("Error writing message:", err)
			break
		}
	}
}

type Message struct {
	Account  int    `json:"account"`
	Password string `json:"password"`
	Data     string `json:"data"`
	Code     int    `json:"code"`
}

func (web *webServer) login(msg *Message) (string, error) {

	opts := gen.RemoteSpawnOptions{
		Name: fmt.Sprintf("player_remote_%d", msg.Account),
	}
	sta := state.NewStateModel(msg.Account)
	sta.PlayerId = msg.Account
	store, err := sta.GetAllState(web.DB)
	if err != nil {
		return "", err
	}
	if store.Pid != "" || store.Status == common.RoleStatusLogin {
		return "disable repeat login", nil
	}

	// todo : rand a gamer node
	gotPid, err := web.process.RemoteSpawn("Gamer@localhost", "player_remote", opts, msg)
	if err != nil {
		return "", err
	}

	log.Logger.Infof("OK selfName: %s, selfId %s, returnId %d,%s,%s", web.process.Name(), web.process.Self(), gotPid.ID, gotPid.Node, gotPid.String())
	log.Logger.Infof("msg %+v", msg)

	sta.Pid = gotPid.String()
	sta.PlayerId = msg.Account
	sta.Status = common.RoleStatusLogin
	sta.AddState(web.DB)
	return "login successful", nil
}
