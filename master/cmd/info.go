package cmd

import (
	"github.com/spf13/cobra"
	"master/apps/masterapp/node"
	"master/common"
	"master/db"
	"master/log"
)

var infoCmd = &cobra.Command{
	Use:   "info [monitor_list] [status]",
	Short: "本地详情服务",
	Long: `
	ERGO-FRAME
==========================================
	| INFO |
		1. info monitor_list 监视器列表
					|-- online 在线
					|-- offline 下线
					|-- disable 禁用
					|-- all 所有
==========================================
`,
	Run: func(cmd *cobra.Command, args []string) {
		if len(args) != 2 {
			log.Logger.Error("param length mismatch")
			return
		}
		db, err := db.InfDb.NewDBClient(&db.SctSSdb{})
		if err != nil {
			log.Logger.Errorf("%+v", err)
		}
		defer db.Close()

		param1 := args[0]
		param2 := args[1]
		switch param1 {
		case "monitor_list":
			nd := node.NewNodesModel()
			switch param2 {
			case "all":
				nodes, err := nd.GetAllNode(db)
				if err != nil {
					log.Logger.Error(err)
				}
				log.Logger.Infof("monitor list: %+v", nodes)
			case "online":
				nodes, err := nd.GetNodesByStatus(db, common.NodeStatusOnline)
				if err != nil {
					log.Logger.Error(err)
				}
				log.Logger.Infof("monitor online list: %+v", nodes)
			case "offline":
				nodes, err := nd.GetNodesByStatus(db, common.NodeStatusOffLine)
				if err != nil {
					log.Logger.Error(err)
				}
				log.Logger.Infof("monitor offline list: %+v", nodes)
			case "disable":
				nodes, err := nd.GetNodesByStatus(db, common.NodeStatusDisable)
				if err != nil {
					log.Logger.Error(err)
				}
				log.Logger.Infof("monitor disable list: %+v", nodes)
			}

		default:
			log.Logger.Error("param1 mismatch")
		}

	},
}

func init() {
	rootCmd.AddCommand(infoCmd)
}
