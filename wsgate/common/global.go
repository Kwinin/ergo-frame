package common

import (
	"wsgate/apps/wsgateapp/db"
	"wsgate/config"
)

type GbVar struct {
	NodeName string
	Cfg      config.Conf
	DB       *db.DBClient
}

type TransMessage struct {
	Msg           interface{}
	CMD           string
	FromNode      config.NodeConf
	FromGenServer string
}
