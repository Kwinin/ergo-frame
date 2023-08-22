package nodes

import (
	"fmt"
	"github.com/fatih/structs"
	"master/db"
)

type NodeModel struct {
	Id     int32
	Role   string
	Name   string
	Type   string
	Addr   string
	Status int
}

func newNodeModel(nodeType string) *NodeModel {
	nd := new(NodeModel)
	nd.Type = nodeType
	return nd
}

func NewNodeModel(nodeType string) *NodeModel {
	return newNodeModel(nodeType)
}

func (nd *NodeModel) TableName() string {
	return fmt.Sprintf("node_model:%s", nd.Type)
}

func (nd *NodeModel) AddNode(db *db.DBClient) error {
	nodeData := structs.Map(nd)
	return db.MultiHSet(nd.TableName(), nodeData)
}

func (nd *NodeModel) DelKeyNode(db *db.DBClient, key string) error {
	return db.HDel(nd.TableName(), key)
}

func (nd *NodeModel) GetNodeInfo(db *db.DBClient) (*NodeModel, error) {
	data, err := db.HGetAll(nd.TableName())
	if err != nil {
		return nil, err
	}
	nd.Id = data["Id"].Int32()
	nd.Role = data["Role"].String()
	nd.Name = data["Name"].String()
	nd.Type = data["Type"].String()
	nd.Addr = data["Addr"].String()
	nd.Status = data["Status"].Int()
	return nd, nil
}

func (nd *NodeModel) ClearNode(db *db.DBClient) error {
	return db.HClear(nd.TableName())
}
