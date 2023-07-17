package mod

import (
	"runtime"
	"time"
)

type Shop struct {
}

func (s Shop) Name() string {
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		logger.Infof("当前文件名:%s", filename)
	} else {
		logger.Info("无法获取当前文件名")
	}
	return filename
}

func (a Shop) OnDate() string {
	return time.Now().String()
}
