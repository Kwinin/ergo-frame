package cmd

import (
	"github.com/spf13/cobra"
	"master/server"
)

var startCmd = &cobra.Command{
	Use:   "start",
	Short: "启动服务",
	Long: `	
	ERGO-FRAME
==========================================
	| START |
==========================================
	-t ---使用文档
	`,
	Run: func(cmd *cobra.Command, args []string) {
		server.StartServer()
	},
}

func init() {
	rootCmd.AddCommand(startCmd)
	startCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}
