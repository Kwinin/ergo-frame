package cmd

import (
	"github.com/spf13/cobra"
	"master/apps/masterapp/node"
	"master/db"
	"master/log"
)

var infoCmd = &cobra.Command{
	Use:   "info [monitor_list]",
	Short: "本地详情服务",
	Long: `
	ERGO-FRAME
==========================================
	| INFO |
	Param1:
		monitor_list: 监视器列表
==========================================
`,
	Run: func(cmd *cobra.Command, args []string) {
		if len(args) != 1 {
			log.Logger.Error("param length mismatch")
			return
		}
		db, err := db.InfDb.NewDBClient(&db.SctSSdb{})
		if err != nil {
			log.Logger.Errorf("%+v", err)
		}
		defer db.Close()

		param1 := args[0]
		switch param1 {
		case "monitor_list":
			nd := node.NewNodesModel()
			nodes, err := nd.GetAllNode(db)
			if err != nil {
				log.Logger.Error(err)
			}
			log.Logger.Infof("monitor list: %+v", nodes)
		default:
			log.Logger.Error("param1 mismatch")
		}

	},
}

func init() {
	rootCmd.AddCommand(infoCmd)
}
