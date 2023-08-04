package config

import (
	"fmt"
	"github.com/spf13/viper"
)

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

	for _, v := range Cfg.NodeList.Master {
		if v.Name == fmt.Sprintf("%s_%d", ServerCfg.ServerName, ServerCfg.ServerID) {
			ServerCfg.Node = v
		}
	}
	if ServerCfg.Node.Name == "" {
		return fmt.Errorf("no set server current node")
	}

	return nil
}
