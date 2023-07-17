package mod

import (
	"gamer/log"
	"runtime"
	"time"
)

var logger = log.InfLog.GetLogger(log.Logrus{})

type Attr struct {
}

func (a Attr) Name() string {
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		logger.Warnf("当前文件名:%s", filename)
	} else {
		logger.Info("无法获取当前文件名")
	}
	return filename
}

func (a Attr) OnDate() string {
	return time.Now().String()
}
