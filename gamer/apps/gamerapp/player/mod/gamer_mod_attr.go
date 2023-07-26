package mod

import (
	"gamer/common"
	"gamer/log"
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
