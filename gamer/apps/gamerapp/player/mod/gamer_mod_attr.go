package mod

import (
	"fmt"
	"runtime"
	"time"
)

type Attr struct {
}

func (a Attr) Name() string {
	_, filename, _, ok := runtime.Caller(0)
	if ok {
		fmt.Println("当前文件名:", filename)
	} else {
		fmt.Println("无法获取当前文件名")
	}
	return filename
}

func (a Attr) OnDate() string {
	return time.Now().String()
}
