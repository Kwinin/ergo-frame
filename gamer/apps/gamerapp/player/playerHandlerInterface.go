package player

type PlayerHandlerInterface interface {
	BeforeHandler()
	AfterHandler()
	MsgHandler(playerId, msgId int32, buf []byte)
}
