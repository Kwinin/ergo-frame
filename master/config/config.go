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

	for _, v := range Cfg.NodeList {
		if v.Role == ServerCfg.ServerRole && v.Id == ServerCfg.ServerID {
			ServerCfg.Node = v
			break
		}
	}
	if ServerCfg.Node.Name == "" {
		return fmt.Errorf("no set server current node")
	}

	return nil
}
func GetNodeInfo(serverRole string, serverId int32) (nodeC *NodeConf, err error) {
	for _, v := range Cfg.NodeList {
		if v.Id == serverId && v.Role == serverRole {
			nodeC = &v
			err = nil
			break
		} else {
			nodeC = nil
			err = fmt.Errorf("get %s_%d faild", serverRole, serverId)
		}
	}
	return nodeC, err
}
