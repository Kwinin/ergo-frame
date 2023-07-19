package config

import (
	"fmt"
	"github.com/spf13/viper"
	"master/log"
)

var logger = log.InfLog.GetLogger(log.Logrus{})

var Config *viper.Viper

func InitConfig(configPath string) error {
	Config = viper.New()
	Config.SetConfigType("yaml")
	Config.AddConfigPath(configPath)
	Config.SetConfigName("config") // 设置配置文件名（例如：config.yaml，config.json，config.toml 等）

	// 读取初始配置文件
	err := Config.ReadInConfig()
	if err != nil {
		return fmt.Errorf("无法加载初始配置文件：%s \n", err)
	}

	Config.Unmarshal(&Cfg)

	return nil
}
