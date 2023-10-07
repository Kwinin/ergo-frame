package main

import (
	"context"
	"encoding/json"
	"fmt"
	"gamer/helper"
	"gamer/log"
	"gamer/proto/account"
	"github.com/gorilla/websocket"
	"github.com/sirupsen/logrus"
	"google.golang.org/protobuf/proto"
	"net/http"
)

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true
	},
}

func handleConnection(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer conn.Close()

	fmt.Println("Client connected")

	sendctx, sendcancelFunc := context.WithCancel(context.Background())
	defer sendcancelFunc()
	sendchan := make(chan []byte, 1)

	accountname := fmt.Sprintf("%v_%v", "test", 3333)
	password := "123456"

	msg := &account.C2S_Login{
		Account:  accountname,
		Password: password,
	}

	type Message struct {
		Account  int    `json:"account"`
		Password string `json:"password"`
		Data     string `json:"data"`
		Code     int    `json:"code"`
	}

	for {
		_, p, err := conn.ReadMessage()
		if err != nil {
			fmt.Println(err)
			return
		}
		fmt.Printf("Received message: %s\n", p)

		p3 := &Message{}
		if err := json.Unmarshal(p, p3); err != nil {
			log.Logger.Error("消息格式错误")
		}

		if p3.Code == 0 {
			SendToClient(int32(account.MSG_ACCOUNT_MODULE), int32(account.MSG_ACCOUNT_LOGIN), msg, sendchan)
		}

		// Echo the message back to the client
		//if err := conn.WriteMessage(messageType, p); err != nil {
		//	fmt.Println(err)
		//	return
		//}
		select {
		case buf := <-sendchan:
			le := helper.IntToBytes(int32(len(buf)), 2)
			err := conn.WriteMessage(websocket.BinaryMessage, helper.BytesCombine(le, buf))
			if err != nil {
				fmt.Println(111, err)
				return
			}
		case <-sendctx.Done():
			return
		}

	}
}

// //SendToClient 发送消息至客户端
func SendToClient(module int32, method int32, pb proto.Message, sendchan chan []byte) {
	//logrus.Debugf("client send msg [%v] [%v] [%v]", module, method, pb)
	data, err := proto.Marshal(pb)
	if err != nil {
		logrus.Errorf("proto encode error[%v] [%v][%v] [%v]", err.Error(), module, method, pb)
		return
	}
	moduleBuf := helper.IntToBytes(module, 2)
	methodBuf := helper.IntToBytes(method, 2)
	sendchan <- helper.BytesCombine(moduleBuf, methodBuf, data)
}

func main() {
	http.HandleFunc("/ws", handleConnection)
	fmt.Println("WebSocket server is running on :8080")
	http.ListenAndServe(":8080", nil)
}
