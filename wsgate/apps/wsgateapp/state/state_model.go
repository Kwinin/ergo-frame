package state

import (
	"github.com/ergo-services/ergo/etf"
	"github.com/fatih/structs"
	"wsgate/apps/wsgateapp/db"
)

type StateModel struct {
	Pid      etf.Pid
	PlayerId string
	Status   int
}

func newStateModel() *StateModel {
	um := new(StateModel)
	return um
}

func NewStateModel() *StateModel {
	return newStateModel()
}

func (state *StateModel) AddState(db *db.DBClient) {
	stateData := structs.Map(state)
	db.MultiHSet("state_model", stateData)
}
