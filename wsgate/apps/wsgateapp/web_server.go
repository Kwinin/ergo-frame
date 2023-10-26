package wsgateapp

import (
	"bytes"
	"context"
	"crypto/tls"
	"encoding/binary"
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/lib"
	"github.com/gorilla/websocket"
	"github.com/sirupsen/logrus"
	"net/http"
	"strconv"
	"wsgate/common"
	"wsgate/config"
	"wsgate/log"
	pbAccount "wsgate/proto/account"
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
	//mux.HandleFunc("/ws", web.handleWebSocketConnection)
	mux.HandleFunc("/ws", web.handleWebSocket)
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
	switch info := message.(type) {
	case etf.Atom:
		switch info {
		case "SocketStop":
			return gen.ServerStatusStopWithReason("stop normal")
		case "timeloop":
			logrus.Debug("time loop")
		}
		web.sendChan <- []byte(info)

	case etf.Tuple:
		fmt.Println(2331)
		//module := info[0].(int32)
		//method := info[1].(int32)
		//buf := info[2].([]byte)
		//web.sendChan <- []byte(info)

	case []byte:
		web.sendChan <- info
	default:
		fmt.Println(32343, info)
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
	CheckOrigin: func(r *http.Request) bool {
		// 允许所有Origin的连接
		return true
	},
}

//	func (web *webServer) handleWebSocketConnection(writer http.ResponseWriter, r *http.Request) {
//		// Upgrade HTTP request to WebSocket connection
//		conn, err := upgrader.Upgrade(writer, r, nil)
//		if err != nil {
//			log.Logger.Error("Error upgrading connection:", err)
//			return
//		}
//		defer conn.Close()
//
//		// 读取消息头
//		var header struct {
//			Length   uint16
//			ModuleID uint16
//			MethodID uint16
//		}
//		for {
//
//			messageType, p, err := conn.ReadMessage()
//			fmt.Println(333, messageType, p, len(p))
//			if err != nil {
//				log.Logger.Error("Error reading message:", err)
//				return
//			}
//
//			if messageType != websocket.BinaryMessage {
//				log.Logger.Error("Unexpected message type")
//				continue
//			}
//
//			err = binary.Read(bytes.NewReader(p[:6]), binary.LittleEndian, &header)
//			if err != nil {
//				log.Logger.Error("Error reading message header:", err)
//
//			}
//			fmt.Println(3434, header.ModuleID, header.MethodID)
//
//			// 读取消息体
//			messageBody := p[6:]
//
//			// Print the received message
//			log.Logger.Infof("Received  module: %d, method %d, messageBody: %s\n", header.ModuleID, header.MethodID, messageBody)
//
//			//switch true {
//			//case msg.Code == 0:
//			//	// 登录
//			//	_, err = web.login(msg)
//			//	if err != nil {
//			//		log.Logger.Error(err)
//			//		break
//			//	}
//			//	fmt.Printf("23434 %s ,%+v \n", web.process.Name(), web.process.Info())
//			//
//			//case msg.Code == 1:
//			//	// 注销
//			//	//sta := state.NewStateModel(msg.Account)
//			//	//store, err := sta.GetAllState(web.DB)
//			//	//if err != nil {
//			//	//	log.Logger.Error(err)
//			//	//}
//			//	//name := fmt.Sprintf("player_remote_%d", store.PlayerId)
//			//
//			//	//module := int32(binary.BigEndian.Uint16(buf[n.Packet : n.Packet+2]))
//			//	//method := int32(binary.BigEndian.Uint16(buf[n.Packet+2 : n.Packet+4]))
//			//
//			//	// todo: rand a gamer node
//			//	err = web.process.Cast(gen.ProcessID{Name: "", Node: "Gamer@localhost"}, etf.Tuple{1000, 1001, message})
//			//	if err != nil {
//			//		log.Logger.Infof("callerr %+v", err)
//			//		break
//			//	}
//			//	//err = sta.ClearState(web.DB)
//			//case msg.Code > 1:
//			//	name := fmt.Sprintf("player_remote_%d", msg.Account)
//			//
//			//	// todo: rand a gamer node
//			//	err := web.process.Cast(gen.ProcessID{Name: name, Node: "Gamer@localhost"}, msg)
//			//	if err != nil {
//			//		log.Logger.Infof("callerr %+v", err)
//			//		break
//			//	}
//			//default:
//			//	break
//			//}
//
//		}
//	}

type Client struct {
	conn     *websocket.Conn
	PlayerId uint16 // 用户标识符
}

var clients = make(map[*Client]bool)

func (web *webServer) handleWebSocket(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println("Error upgrading connection:", err)
		return
	}
	//token := r.Header.Get("Authorization")
	//log.Logger.Infof("token: %s", token)

	// 为每个连接分配一个唯一的用户标识符
	client := &Client{conn: conn}

	sendctx, sendcancelFunc := context.WithCancel(context.Background())

	defer sendcancelFunc()

	defer func() {
		fmt.Println("用户", client.PlayerId, "断开连接")
		delete(clients, client)
		conn.Close()
	}()

	//if token == "" {
	//	// 未提供令牌，拒绝连接
	//	w.WriteHeader(http.StatusUnauthorized)
	//	return
	//}

	var header struct {
		Length   uint16
		PlayerId uint16
		ModuleId uint16
		MethodID uint16
	}

	for {
		messageType, p, err := conn.ReadMessage()
		if err != nil {
			break
		}

		if messageType != websocket.BinaryMessage {
			fmt.Println("Unexpected message type")
			continue
		}

		err = binary.Read(bytes.NewReader(p[:8]), binary.LittleEndian, &header)
		if err != nil {
			log.Logger.Error("Error reading message header:", err)

		}

		log.Logger.Infof("length: %d, PlayerId: %d, MoudleId: %d, MethodId: %d", header.Length, header.PlayerId, header.ModuleId, header.MethodID)
		//
		//var length uint16
		//err = binary.Read(bytes.NewReader(p[:2]), binary.LittleEndian, &length)
		//if err != nil {
		//	log.Logger.Error("Error reading length:", err)
		//}
		//
		//fmt.Printf("length %d \n", length)
		//
		//var moduleID uint16
		//err = binary.Read(bytes.NewReader(p[2:4]), binary.LittleEndian, &moduleID)
		//if err != nil {
		//	log.Logger.Error("Error reading module ID:", err)
		//}
		//
		//fmt.Printf("moduleID %d \n", moduleID)
		//
		//var methodID uint16
		//err = binary.Read(bytes.NewReader(p[4:6]), binary.LittleEndian, &methodID)
		//if err != nil {
		//	log.Logger.Error("Error reading method ID:", err)
		//}
		//
		//fmt.Printf("methodID %d \n", methodID)
		switch true {
		// 登录特殊处理
		case header.MethodID == uint16(pbAccount.MSG_ACCOUNT_LOGIN):
			// 读取消息体
			log.Logger.Infof("messageBody: %+v", header.PlayerId)
			_, err = web.login(header.PlayerId)
			if err != nil {
				log.Logger.Error(err)
				break
			}
			client.PlayerId = header.PlayerId
			clients[client] = true

		default:
			messageBody := p[8:]
			name := fmt.Sprintf("player_remote_%d", header.PlayerId)
			//err := web.process.Send(gen.ProcessID{Name: name, Node: "Gamer@localhost"}, etf.Term(etf.Tuple{etf.Atom("$gen_cast"), etf.Tuple{header.PlayerId, header.MsgID, messageBody}}))
			err = web.process.Cast(gen.ProcessID{Name: name, Node: "Gamer@localhost"}, etf.Tuple{header.PlayerId, header.ModuleId, header.MethodID, messageBody})
			if err != nil {
				log.Logger.Infof("callerr %+v", err)
				break
			}
		}

		select {
		case buf := <-web.sendChan:
			err := conn.WriteMessage(websocket.BinaryMessage, buf)
			if err != nil {
				log.Logger.Error(err)
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

func (web *webServer) login(playerId uint16) (string, error) {

	opts := gen.RemoteSpawnOptions{
		Name: fmt.Sprintf("player_remote_%d", playerId),
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

	playerStr := strconv.FormatUint(uint64(playerId), 10)
	// todo : rand a gamer node
	gotPid, err := web.process.RemoteSpawn("Gamer@localhost", "player_remote", opts, etf.Atom(playerStr))
	if err != nil {
		return "", err
	}

	log.Logger.Infof("OK selfName: %s, selfId %s, returnRemoteId %d,%s,%s", web.process.Name(), web.process.Self(), gotPid.ID, gotPid.Node, gotPid.String())

	//sta.Pid = gotPid.String()
	//sta.PlayerId = msg.Account
	//sta.Status = common.RoleStatusLogin
	//sta.AddState(web.DB)
	return "login successful", nil
}
