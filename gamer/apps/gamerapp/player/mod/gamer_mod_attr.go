package mod

import (
	"fmt"
	"gamer/common"
	"gamer/log"
	pbaccount "gamer/proto/account"
	"runtime"
	"time"
)

type Attr struct {
	common.GbVar
}

func (a Attr) Name() string {
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		log.Logger.Warnf("当前文件名:%s", filename)
	} else {
		log.Logger.Info("无法获取当前文件名")
	}
	return filename
}

func (a Attr) OnDate() string {
	return time.Now().String()
}

func (a Attr) OnMessage(Id int) string {
	return fmt.Sprintf("use id :%d", Id)
}

func (c *Attr) AccountLogin(msg *pbaccount.C2S_Login) {

	fmt.Println(2222, msg.Account, msg.Password)

}
