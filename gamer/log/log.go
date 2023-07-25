package log

import "github.com/sirupsen/logrus"

var Logger *logrus.Logger

type InfLog interface {
	GetLogger() *logrus.Logger
}

//var Logger = InfLog.GetLogger(Logrus{})
