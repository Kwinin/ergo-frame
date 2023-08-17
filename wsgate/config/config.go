package config

import (
	"fmt"
	"github.com/fsnotify/fsnotify"
	"github.com/spf13/viper"
	"wsgate/log"
)

var logger = log.InfLog.GetLogger(log.Logrus{})
var Config *viper.Viper
var Cfg Conf

func InitConfig(configPath string) error {
	Config = viper.New()
	Config.SetConfigType("json")
	Config.AddConfigPath(configPath)
	Config.SetConfigName("config") // 设置配置文件名（例如：config.yaml，config.json，config.toml 等）

	// 读取初始配置文件
	err := Config.ReadInConfig()
	if err != nil {
		return fmt.Errorf("无法加载初始配置文件：%s \n", err)
	}

	Config.Unmarshal(&Cfg)

	if Cfg.Node.Name == fmt.Sprintf("%s_%d", ServerCfg.ServerName, ServerCfg.ServerID) {
		ServerCfg.Node = Cfg.Node
	}

	if ServerCfg.Node.Name == "" {
		return fmt.Errorf("no set server current node")
	}

	// 启用自动热重载
	Config.WatchConfig()
	Config.OnConfigChange(func(e fsnotify.Event) {

		// 重新解析配置文件到结构体
		err := Config.Unmarshal(&Cfg)
		if err != nil {
			logger.Infof("无法重新解析配置文件：%s \n", err)
			return
		}
	})

	// 监听文件系统的变化
	go watchFileSystem(configPath)

	return nil
}

func watchFileSystem(configPath string) {
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		logger.Info("无法启动文件系统监视：", err)
		return
	}
	defer watcher.Close()

	err = watcher.Add(configPath)
	if err != nil {
		logger.Info("无法添加监视目录：", err)
		return
	}

	for {
		select {
		case event, ok := <-watcher.Events:
			if !ok {
				return
			}
			if event.Op&fsnotify.Write == fsnotify.Write {
				logger.Info("配置文件被修改：", event.Name)
				// 触发配置文件更新操作
				Config.ReadInConfig()
			}
		case err, ok := <-watcher.Errors:
			if !ok {
				return
			}
			logger.Info("文件系统监视错误：", err)
		}
	}
}
