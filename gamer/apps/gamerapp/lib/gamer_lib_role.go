package lib

import (
	"fmt"
	"github.com/ergo-services/ergo/etf"
	"github.com/ergo-services/ergo/gen"
)

type RoleLib struct {
}

type RoleTag struct {
	Tag    string
	RoleId int64
}

func (rl *RoleLib) LaunchRolePid(process *gen.ServerProcess, roleTag RoleTag, object gen.ProcessBehavior, args ...etf.Term) gen.Process {
	name := fmt.Sprintf("%s_%d", roleTag.Tag, roleTag.RoleId)
	p, _ := process.Spawn(name, gen.ProcessOptions{}, object, args...)
	return p
}