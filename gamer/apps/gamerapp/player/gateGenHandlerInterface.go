package player

import (
	"time"

	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

type GateGenHandlerInterface interface {
	InitHandler(process *gen.ServerProcess, sendChan chan []byte)
	LoopHandler() (nextLoop time.Duration)
	MsgHandler(module, method int32, buf []byte)
	HandleCall(message etf.Term)
	HandleInfo(message etf.Term)
	Terminate(reason string)
	GenServerStatus() gen.ServerStatus
}
