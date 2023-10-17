package state

import (
	"fmt"
	"github.com/fatih/structs"
	"wsgate/db"
)

type StateModel struct {
	Pid      string
	PlayerId int32
	Status   int
}

func newStateModel(playerId int32) *StateModel {
	um := new(StateModel)
	um.PlayerId = playerId
	return um
}

func NewStateModel(playerId int32) *StateModel {
	return newStateModel(playerId)
}

func (state *StateModel) TableName() string {
	return fmt.Sprintf("state_model:%d", state.PlayerId)
}

func (state *StateModel) AddState(db *db.DBClient) error {
	stateData := structs.Map(state)
	return db.MultiHSet(state.TableName(), stateData)
}

func (state *StateModel) GetAllState(db *db.DBClient) (*StateModel, error) {
	data, err := db.HGetAll(state.TableName())
	if err != nil {
		return nil, err
	}
	state.PlayerId = data["PlayerId"].Int32()
	state.Status = data["Status"].Int()
	state.Pid = data["Pid"].String()

	//var pid string
	//err = json.Unmarshal([]byte(data["Pid"].String()), &pid)
	//if err != nil {
	//	return nil, fmt.Errorf("error decoding store.Pid: %s", err.Error())
	//}
	//store.Pid = pid
	return state, nil
}

func (state *StateModel) ClearState(db *db.DBClient) error {
	return db.HClear(state.TableName())
}
