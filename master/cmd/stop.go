package cmd

import (
	"github.com/spf13/cobra"
	"master/common"
	"master/config"
	"master/log"
	"strconv"
)

var stopCmd = &cobra.Command{
	Use:   "stop [serverName] [serverId]",
	Short: "关闭服务",
	Long: `
	ERGO-FRAME
==========================================
	| STOP |
==========================================
`,
	Run: func(cmd *cobra.Command, args []string) {
		var serverId int32
		var serverName string
		if len(args) == 2 {
			serverName = args[0]
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
		gen := NewDebugGen("stop", serverName, serverId)

		if info, err := gen.Call(common.Shutdown); err == nil {
			log.Logger.Infof("[%v] shutdown  \n", info)
		} else {
			log.Logger.Infof("err: %v", err)
		}

	},
}

func init() {
	rootCmd.AddCommand(stopCmd)
}
