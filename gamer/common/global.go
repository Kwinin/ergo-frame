package common

import (
	"gamer/apps/gamerapp/db"
	"gamer/config"
	"github.com/ergo-services/ergo/etf"
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

type TransMessageEtf struct {
	Msg           etf.Term
	CMD           etf.Atom
	FromNode      etf.Map
	FromGenServer etf.Atom
}
