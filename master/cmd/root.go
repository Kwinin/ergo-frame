package cmd

import (
	"fmt"
	"master/config"
	"master/log"
	"os"

	"github.com/spf13/cobra"
)

var rootCmd = &cobra.Command{
	Use:   "root demo",
	Short: "root Short",
	Long: `
        ERGO-FRAME
==========================================
        | ROOT |
==========================================
        -t ---使用文档

`,
}

var cfgFile string

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/cfg.yaml)")

	rootCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")

}

func initConfig() {
	log.InitLogger()

	if cfgFile == "" {
		cfgFile = config.ServerCfg.CfgPath
	}

	err := config.InitConfig(cfgFile)
	if err != nil {
		log.Logger.Errorf("error config file %v", err)
	}
	log.Logger.Info("initConfig config :", cfgFile)

}
