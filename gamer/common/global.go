package common

import (
	"gamer/apps/gamerapp/db"
	"gamer/config"
)

type GbVar struct {
	Name string
	Cfg  config.Conf
	DB   *db.DBClient
}
