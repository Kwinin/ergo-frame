package player

type PlayerHandlerInterface interface {
	BeforeHandler()
	AfterHandler()
	OfflineHandler()
	MsgHandler(playerId, msgId int32, buf []byte)
}
