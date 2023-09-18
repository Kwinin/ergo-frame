package cmd

import (
	"github.com/spf13/cobra"
	"master/common"
	"master/config"
	"master/log"
	"strconv"
)

var stopCmd = &cobra.Command{
	Use:   "stop [serverRole] [serverId]",
	Short: "关闭服务",
	Long: `
	ERGO-FRAME
==========================================
	| STOP |
==========================================
`,
	Run: func(cmd *cobra.Command, args []string) {
		var serverId int32
		var serverRole string
		if len(args) == 2 {
			serverRole = args[0]
			val, err := strconv.Atoi(args[1])
			if err != nil {
				log.Logger.Info("转换失败：", err)
				return
			}
			serverId = int32(val)
		} else {
			serverId = config.ServerCfg.ServerID
			serverRole = config.ServerCfg.ServerRole
		}
		gen := NewDebugGen("stop", serverRole, serverId)

		if info, err := gen.Call(common.Shutdown); err == nil {
			log.Logger.Infof("[%v] shutdown  \n", info)
		} else {
			log.Logger.Infof("err: %v", err)
		}

		//err := gen.Send(common.Shutdown)
		//if err != nil {
		//	log.Logger.Infof("err: %v", err)
		//}

	},
}

func init() {
	rootCmd.AddCommand(stopCmd)
}
