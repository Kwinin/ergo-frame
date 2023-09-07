package cmd

import (
	"github.com/spf13/cobra"
	"master/config"
	"master/log"
	"strconv"
)

var monitorCmd = &cobra.Command{
	Use:   "monitor [serverName] [serverId]",
	Short: "监听服务",
	Long: `
	ERGO-FRAME
==========================================
	| MONITOR |
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
		gen := NewDebugGen("monitor", serverName, serverId)

		gen.Monitor()

	},
}

func init() {
	rootCmd.AddCommand(monitorCmd)
}
