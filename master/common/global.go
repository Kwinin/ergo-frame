package common

import "master/config"

type TransMessage struct {
	CMD  string          `json:"cmd"`
	From config.NodeConf `json:"node"`
}
