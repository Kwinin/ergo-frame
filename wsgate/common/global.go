package common

import (
	"wsgate/config"
	"wsgate/db"
)

type GbVar struct {
	NodeName string
	Cfg      config.ServerConfig
	DB       *db.DBClient
}

type TransMessage struct {
	Msg           interface{}
	CMD           string
	FromNode      config.NodeConf
	FromGenServer string
}
