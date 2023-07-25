package log

import (
	"fmt"
	"github.com/sirupsen/logrus"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	//"gopkg.in/natefinch/lumberjack.v2"
)

type Logrus struct {
}

func InitLogger() {
	Logger = logrus.New()

	// 设置日志输出格式为JSON格式
	Logger.SetFormatter(&CustomFormatter{
		TimestampFormat: "2006-01-02 15:04:05.000000",
	})

	// 设置日志输出目标为文件和控制台
	file, err := os.OpenFile("app.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err == nil {
		//// 使用lumberjack进行日志分片
		//log.SetOutput(&lumberjack.Logger{
		//	Filename:   "app.log",
		//	MaxSize:    100,   // 单个日志文件的最大尺寸，单位：MB
		//	MaxBackups: 3,     // 保留的旧日志文件最大数量
		//	MaxAge:     30,    // 保留的旧日志文件最大天数
		//	LocalTime:  true,  // 使用本地时间命名旧日志文件
		//	Compress:   false, // 是否压缩旧日志文件
		//})
		// 创建MultiWriter，同时输出到文件和控制台
		mw := io.MultiWriter(file, os.Stdout)
		Logger.SetOutput(mw)
	} else {
		Logger.Info("无法打开日志文件，日志将仅输出到控制台")
	}

	// 添加日志钩子，记录文件位置
	Logger.AddHook(NewCallerHook())
}

// 获取日志实例
func (l Logrus) GetLogger() *logrus.Logger {
	return Logger
}

// 自定义日志钩子，记录文件位置
type CallerHook struct{}

func NewCallerHook() *CallerHook {
	return &CallerHook{}
}

func (hook *CallerHook) Levels() []logrus.Level {
	return logrus.AllLevels
}

func (hook *CallerHook) Fire(entry *logrus.Entry) error {
	pc := make([]uintptr, 3)
	runtime.Callers(7, pc) // 调整参数以适配实际情况

	// 迭代查找项目文件位置
	var file string
	var line int
	for _, p := range pc {
		fn := runtime.FuncForPC(p).Name()
		if !strings.Contains(fn, "github.com/sirupsen/") {
			file, line = runtime.FuncForPC(p).FileLine(p)
			break
		}
	}
	if file != "" {
		// 从完整文件路径中提取文件名
		fileName := filepath.Base(file)
		entry.Data["file"] = fileName
		entry.Data["line"] = line
	}
	return nil
}

type CustomFormatter struct {
	TimestampFormat string
}

func (f *CustomFormatter) Format(entry *logrus.Entry) ([]byte, error) {
	timestamp := entry.Time.Format(f.TimestampFormat)
	//2023-07-14 15:58:55.209 [info] <0.2115.0> [tcp_handler_98] terminate: <0.2115.0>, playerid: 10002929, playerpid: <15480.744.8>
	// 构建输出的字符串
	msg := fmt.Sprintf("%s [%s] [%s_%d] %s\n", timestamp, entry.Level, entry.Data["file"], entry.Data["line"], entry.Message)

	return []byte(msg), nil
}
