package wsgateapp

import (
	"context"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/lib"
	"github.com/gorilla/websocket"
	"github.com/sirupsen/logrus"
	"net/http"
	"wsgate/common"
	"wsgate/config"
	"wsgate/log"
)

func createWebActor(gbVar common.GbVar) gen.ServerBehavior {
	return &webServer{
		GbVar: common.GbVar{
			NodeName: gbVar.NodeName,
			Cfg:      gbVar.Cfg,
			DB:       gbVar.DB,
		},
		sendChan: make(chan []byte, 1),
	}
}

type webServer struct {
	gen.Web
	common.GbVar
	process  *gen.WebProcess
	sendChan chan []byte
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

func (web *webServer) HandleWebCall(process *gen.WebProcess, from gen.ServerFrom, message etf.Term) (etf.Term, gen.ServerStatus) {
	log.Logger.Infof("HandleWebCall: unhandled message (from %#v) %#v", from, message)
	return etf.Atom("ok"), gen.ServerStatusOK
}

// HandleWebCast
func (web *webServer) HandleWebCast(process *gen.WebProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleWebCast: 1111 unhandled message %#v", message)

	switch info := message.(type) {
	case etf.Atom:
		switch info {
		case "SocketStop":
			return gen.ServerStatusStopWithReason("stop normal")
		case "timeloop":
			logrus.Debug("time loop")
		}
		fmt.Println(233, info)
		web.sendChan <- []byte(info)

	case etf.Tuple:
		fmt.Println(2331)

		//module := info[0].(int32)
		//method := info[1].(int32)
		//buf := info[2].([]byte)

	case []byte:
		fmt.Println(2333)

		logrus.Debug("[]byte:", info)
	}
	return gen.ServerStatusOK
}

// HandleWebInfo
func (web *webServer) HandleWebInfo(process *gen.WebProcess, message etf.Term) gen.ServerStatus {
	log.Logger.Infof("HandleWebInfo: unhandled message %#v", message)
	return gen.ServerStatusOK
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

	sendctx, sendcancelFunc := context.WithCancel(context.Background())
	defer sendcancelFunc()

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

		switch true {
		case msg.Code == 0:
			// 登录
			_, err = web.login(msg)
			if err != nil {
				log.Logger.Error(err)
				break
			}
			fmt.Printf("23434 %s ,%+v \n", web.process.Name(), web.process.Info())

		case msg.Code == 1:
			// 注销
			//sta := state.NewStateModel(msg.Account)
			//store, err := sta.GetAllState(web.DB)
			//if err != nil {
			//	log.Logger.Error(err)
			//}
			//name := fmt.Sprintf("player_remote_%d", store.PlayerId)

			//module := int32(binary.BigEndian.Uint16(buf[n.Packet : n.Packet+2]))
			//method := int32(binary.BigEndian.Uint16(buf[n.Packet+2 : n.Packet+4]))

			// todo: rand a gamer node
			err = web.process.Cast(gen.ProcessID{Name: "", Node: "Gamer@localhost"}, etf.Tuple{1000, 1001, message})
			if err != nil {
				log.Logger.Infof("callerr %+v", err)
				break
			}
			//err = sta.ClearState(web.DB)
		case msg.Code > 1:
			name := fmt.Sprintf("player_remote_%d", msg.Account)

			// todo: rand a gamer node
			err := web.process.Cast(gen.ProcessID{Name: name, Node: "Gamer@localhost"}, msg)
			if err != nil {
				log.Logger.Infof("callerr %+v", err)
				break
			}
		default:
			break
		}

		select {
		case buf := <-web.sendChan:
			err := conn.WriteMessage(websocket.BinaryMessage, buf)
			if err != nil {
				fmt.Println(111, err)
				break
			}
		case <-sendctx.Done():
			break
		}
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
		//Name: "player_remote",
	}
	//sta := state.NewStateModel(msg.Account)
	//sta.PlayerId = msg.Account
	//store, err := sta.GetAllState(web.DB)
	//if err != nil {
	//	return "", err
	//}
	//if store.Pid != "" || store.Status == common.RoleStatusLogin {
	//	return "disable repeat login", nil
	//}

	// todo : rand a gamer node
	gotPid, err := web.process.RemoteSpawn("Gamer@localhost", "player_remote", opts, msg)
	if err != nil {
		return "", err
	}

	log.Logger.Infof("OK selfName: %s, selfId %s, returnRemoteId %d,%s,%s", web.process.Name(), web.process.Self(), gotPid.ID, gotPid.Node, gotPid.String())
	log.Logger.Infof("msg %+v", msg)

	//sta.Pid = gotPid.String()
	//sta.PlayerId = msg.Account
	//sta.Status = common.RoleStatusLogin
	//sta.AddState(web.DB)
	return "login successful", nil
}
