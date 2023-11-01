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
	"google.golang.org/protobuf/proto"
	"math/rand"
	"net/http"
	"strconv"
	"wsgate/apps/wsgateapp/node"
	"wsgate/apps/wsgateapp/state"
	"wsgate/common"
	"wsgate/config"
	"wsgate/helper"
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

	options.Port = uint16(config.ServerCfg.Web.Port)
	options.Host = config.ServerCfg.Web.Host
	proto := "http"
	if config.ServerCfg.Web.Enable {
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

type Client struct {
	conn     *websocket.Conn
	PlayerId uint32
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
		PlayerId uint32
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

		err = binary.Read(bytes.NewReader(p[:10]), binary.LittleEndian, &header)
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
		messageBody := p[10:]
		switch true {
		// 登录特殊处理
		case header.MethodID == uint16(pbAccount.MSG_ACCOUNT_LOGIN):
			// 读取消息体
			log.Logger.Infof("messageBody: %+v", header.PlayerId)

			sta := state.NewStateModel(header.PlayerId)
			store, err := sta.GetAllState(web.DB)
			if store.PlayerId != 0 || store.Status == common.RoleStatusOnline {
				rspMsg := &pbAccount.Msg_1001Rsp{
					RetCode: 2,
					Data:    "login failed",
				}
				web.SendToClient(int32(pbAccount.MSG_ACCOUNT_MODULE), int32(pbAccount.MSG_ACCOUNT_LOGIN), rspMsg)
				break
			}

			_, err = web.login(header.PlayerId)
			if err != nil {
				log.Logger.Error(err)
				rspMsg := &pbAccount.Msg_1001Rsp{
					RetCode: 2,
					Data:    err.Error(),
				}
				web.SendToClient(int32(pbAccount.MSG_ACCOUNT_MODULE), int32(pbAccount.MSG_ACCOUNT_LOGIN), rspMsg)
				break
			}
			client.PlayerId = header.PlayerId
			clients[client] = true
		case header.MethodID == uint16(pbAccount.MSG_ACCOUNT_OFFLINE):
			sta := state.NewStateModel(header.PlayerId)
			store, err := sta.GetAllState(web.DB)
			if store.Status != common.RoleStatusOnline {
				rspMsg := &pbAccount.Msg_1002Rsp{
					RetCode: 2,
					Data:    "no online",
				}
				web.SendToClient(int32(pbAccount.MSG_ACCOUNT_MODULE), int32(pbAccount.MSG_ACCOUNT_OFFLINE), rspMsg)
			}
			name := fmt.Sprintf("player_remote_%d", header.PlayerId)
			err = web.process.Cast(gen.ProcessID{Name: name, Node: store.NodeAddr}, etf.Tuple{header.PlayerId, header.ModuleId, header.MethodID, messageBody})
			if err != nil {
				log.Logger.Infof("cast err %+v", err)
				break
			}

			err = sta.ClearState(web.DB)
			if err != nil {
				break
			}

		default:
			name := fmt.Sprintf("player_remote_%d", header.PlayerId)
			//err := web.process.Send(gen.ProcessID{Name: name, Node: "Gamer@localhost"}, etf.Term(etf.Tuple{etf.Atom("$gen_cast"), etf.Tuple{header.PlayerId, header.MsgID, messageBody}}))
			sta := state.NewStateModel(header.PlayerId)
			stateModel, err := sta.GetAllState(web.DB)
			fmt.Println(3434, stateModel)
			if err != nil {
				break
			}
			err = web.process.Cast(gen.ProcessID{Name: name, Node: stateModel.NodeAddr}, etf.Tuple{header.PlayerId, header.ModuleId, header.MethodID, messageBody})
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

func (web *webServer) login(playerId uint32) (string, error) {
	opts := gen.RemoteSpawnOptions{
		Name: fmt.Sprintf("player_remote_%d", playerId),
		//Name: "player_remote",
	}

	playerStr := strconv.FormatUint(uint64(playerId), 10)
	nd := node.NewNodesModel()
	nodes, err := nd.GetNodesByRole(web.DB, "gamer")
	if err != nil {
		return "", err
	}
	randomIndex := rand.Intn(len(nodes))
	randomElement := nodes[randomIndex]

	gotPid, err := web.process.RemoteSpawn(randomElement.Addr, "player_remote", opts, etf.Atom(playerStr))
	if err != nil {
		return "", err
	}

	log.Logger.Infof("OK selfName: %s, selfId %s, returnRemoteId %d,%s,%s", web.process.Name(), web.process.Self(), gotPid.ID, gotPid.Node, gotPid.String())
	sta := state.NewStateModel(playerId)
	sta.Pid = gotPid.String()
	sta.Status = common.RoleStatusOnline
	sta.NodeAddr = randomElement.Addr
	err = sta.AddState(web.DB)
	if err != nil {
		name := fmt.Sprintf("player_remote_%d", sta.PlayerId)
		err = web.process.Cast(gen.ProcessID{Name: name, Node: sta.NodeAddr}, etf.Tuple{sta.PlayerId, pbAccount.MSG_ACCOUNT_MODULE, pbAccount.MSG_ACCOUNT_OFFLINE, "wsgate register failed"})
		return "", err
	}
	return "login successful", nil
}

func (web *webServer) SendToClient(module int32, method int32, pb proto.Message) {
	//logrus.Debugf("client send msg [%v] [%v] [%v]", module, method, pb)
	data, err := proto.Marshal(pb)
	if err != nil {
		logrus.Errorf("proto encode error[%v] [%v][%v] [%v]", err.Error(), module, method, pb)
		return
	}

	moduleBuf := helper.IntToBytes(module, 2)
	methodBuf := helper.IntToBytes(method, 2)
	web.sendChan <- helper.BytesCombine(moduleBuf, methodBuf, data)
}
