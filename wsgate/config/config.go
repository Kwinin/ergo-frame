package config

import (
	"fmt"
	"github.com/fsnotify/fsnotify"
	"github.com/spf13/viper"
	"wsgate/log"
)

var Config *viper.Viper

var ServerCfg ServerConfig

func InitConfig(configPath string) error {
	Config = viper.New()
	Config.SetConfigType("json")
	Config.AddConfigPath(configPath)
	// 读取初始配置文件
	Config.SetConfigName("sysConfig")
	err := Config.ReadInConfig()
	if err != nil {
		return fmt.Errorf("无法加载初始配置文件：%s\n", err)
	}
	// 合并其他配置文件
	if err = Config.MergeInConfig(); err != nil {
		return fmt.Errorf("无法合并配置文件：%s\n", err)
	}
	if err := Config.Unmarshal(&ServerCfg); err != nil {
		return fmt.Errorf("无法解析配置到Cfg：%s\n", err)
	}

	log.Logger.Infof("cfg  info id: %d/%d ; role: %s/%s", ServerCfg.Node.Id, ServerCfg.ServerID, ServerCfg.Node.Role, ServerCfg.ServerRole)
	if ServerCfg.Node.Id != ServerCfg.ServerID || ServerCfg.Node.Role != ServerCfg.ServerRole {
		return fmt.Errorf("no set server current node")
	}

	//启用自动热重载
	//Config.WatchConfig()
	//Config.OnConfigChange(func(e fsnotify.Event) {
	//
	//	// 重新解析配置文件到结构体
	//	err := Config.Unmarshal(&ServerCfg)
	//	if err != nil {
	//		log.Logger.Infof("无法重新解析配置文件：%s \n", err)
	//		return
	//	}
	//})
	//
	//// 监听文件系统的变化
	//go watchFileSystem(configPath)

	return nil
}

func watchFileSystem(configPath string) {
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		log.Logger.Info("无法启动文件系统监视：", err)
		return
	}
	defer watcher.Close()

	err = watcher.Add(configPath)
	if err != nil {
		log.Logger.Info("无法添加监视目录：", err)
		return
	}

	for {
		select {
		case event, ok := <-watcher.Events:
			if !ok {
				return
			}
			if event.Op&fsnotify.Write == fsnotify.Write {
				log.Logger.Info("配置文件被修改：", event.Name)
				// 触发配置文件更新操作
				Config.ReadInConfig()
			}
		case err, ok := <-watcher.Errors:
			if !ok {
				return
			}
			log.Logger.Info("文件系统监视错误：", err)
		}
	}
}
