package player

import (
	"github.com/ergo-services/ergo/gen"
)

type GateGenHandlerInterface interface {
	InitHandler(process *gen.ServerProcess, sendChan chan []byte)
	MsgHandler(playerId, msgId int32, buf []byte)
}
