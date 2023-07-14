package mod

import (
	"fmt"
	"runtime"
	"time"
)

type Shop struct {
}

func (s Shop) Name() string {
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		fmt.Println("当前文件名:", filename)
	} else {
		fmt.Println("无法获取当前文件名")
	}
	return filename
}

func (a Shop) OnDate() string {
	return time.Now().String()
}
