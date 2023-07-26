package wsgateapp

import (
	"crypto/tls"
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
	"github.com/ergo-services/ergo/lib"
	"github.com/gorilla/websocket"
	"net/http"
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
		web.DB.Set("kwinin", string(message))
		// Send a response back to the client
		response := "This is the server response."
		err = conn.WriteMessage(websocket.TextMessage, []byte(response))
		if err != nil {
			fmt.Println("Error writing message:", err)
			break
		}
	}
}
