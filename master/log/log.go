package log

import "github.com/sirupsen/logrus"

var log *logrus.Logger

type InfLog interface {
	GetLogger() *logrus.Logger
}
