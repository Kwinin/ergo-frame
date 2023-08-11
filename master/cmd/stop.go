package cmd

import (
	"github.com/spf13/cobra"
	"master/common"
	"master/config"
	"master/log"
	"strconv"
)

var stopCmd = &cobra.Command{
	Use:   "stop",
	Short: "关闭服务",
	Long:  `shut down game server`,
	Run: func(cmd *cobra.Command, args []string) {
		var serverId int32
		var serverName common.GenServerName
		if len(args) == 3 {
			serverName = common.GenServerName(args[0])
			val, err := strconv.Atoi(args[1])
			if err != nil {
				log.Logger.Info("转换失败：", err)
				return
			}
			serverId = int32(val)
		} else {
			serverId = config.ServerCfg.ServerID
			serverName = config.ServerCfg.ServerName
		}
		startDebugGen(serverName, serverId)

		if info, err := call("shutdown"); err == nil {
			log.Logger.Infof("[%v] shutdown  \n", info)
		} else {
			log.Logger.Infof("err: %v", err)
		}

	},
}

func init() {
	rootCmd.AddCommand(stopCmd)
}
