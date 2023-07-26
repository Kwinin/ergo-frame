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
