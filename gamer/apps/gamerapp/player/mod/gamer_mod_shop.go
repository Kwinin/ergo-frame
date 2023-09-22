package mod

import (
	"fmt"
	"gamer/common"
	"gamer/log"
	"runtime"
	"time"
)

type Shop struct {
	common.GbVar
}

func (s Shop) Name() string {
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		log.Logger.Infof("当前文件名:%s", filename)
	} else {
		log.Logger.Info("无法获取当前文件名")
	}

	result, _ := s.DB.Get("my_key")

	log.Logger.Infof("******* %v", result)
	return filename
}

func (a Shop) OnDate() string {
	return time.Now().String()
}

func (a Shop) OnMessage(Id int) string {
	return fmt.Sprintf("use id :%d", Id)
}
