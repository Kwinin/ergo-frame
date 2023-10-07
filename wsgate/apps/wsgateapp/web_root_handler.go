package wsgateapp

import (
	"encoding/json"
	"fmt"
	"github.com/ergo-services/ergo/gen"
	"wsgate/log"
)

type rootHandler struct {
	gen.WebHandler
}

//type Message struct {
//	Account  string `json:"account"`
//	Password string `json:"password"`
//	Data     string `json:"data"`
//}

func (r *rootHandler) HandleRequest(process *gen.WebHandlerProcess, request gen.WebMessageRequest) gen.WebHandlerStatus {
	request.Response.Write([]byte("Hello"))

	var msg *Message
	err := json.NewDecoder(request.Request.Body).Decode(&msg)
	if err != nil {
		log.Logger.Error(err)
	}

	fmt.Printf("Name: %+v \n", msg)
	r.login(process, msg)

	return gen.WebHandlerStatusDone
}

func (r *rootHandler) login(process *gen.WebHandlerProcess, msg *Message) {
	opts := gen.RemoteSpawnOptions{
		Name: fmt.Sprintf("player_remote_%s", msg.Account),
	}

	gotPid, err := process.RemoteSpawn("Gamer@localhost", "player_remote", opts, msg.Account, msg.Data)
	if err != nil {
		log.Logger.Error(err)
	}
	log.Logger.Infof("OK selfName: %s, selfId %s, returnId %d,%s", process.Name(), process.Self(), gotPid.ID, gotPid.Node)
	log.Logger.Infof("msg %+v", msg)
	//sta := state.NewStateModel(msg.Account)
	//store, err := sta.GetAllState(web.DB)
	//if err != nil {
	//	log.Logger.Error(err)
	//}
	//if store.PlayerId != msg.Account {
	//	log.Logger.Infof("login failed")
	//	return
	//}
	//log.Logger.Infof("kwinin %+v", store)
	//sta.Pid = gotPid.String()
	//sta.PlayerId = msg.Account
	//sta.Status = 1
	//sta.AddState(r.DB)

	//web.DB.Set("kwinin", string(message))
}
