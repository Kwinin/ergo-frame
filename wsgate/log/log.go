package log

import "github.com/sirupsen/logrus"

type InfLog interface {
	GetLogger() *logrus.Logger
}
