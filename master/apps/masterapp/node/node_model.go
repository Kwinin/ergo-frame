package node

import (
	"encoding/json"
	"fmt"
	"master/db"
)

type NodesModel struct {
	Id     int32
	Role   string
	Name   string
	Addr   string
	Status int
}

func newNodesModel() *NodesModel {
	nd := new(NodesModel)
	return nd
}

func NewNodesModel() *NodesModel {
	return newNodesModel()
}

func (nd *NodesModel) TableName() string {
	return "nodes_model"
}

func (nd *NodesModel) SetOneNode(db *db.DBClient, newNode NodesModel) error {
	nodes, err := nd.GetAllNode(db)
	for _, v := range nodes {
		if newNode.Id == v.Id || newNode.Addr == v.Addr {
			return fmt.Errorf("the same value %d/%s in DB", newNode.Id, newNode.Addr)
		}
	}

	data := append(nodes, newNode)

	jsonData, err := json.Marshal(data)
	if err != nil {
		return err
	}
	db.Set(nd.TableName(), string(jsonData))
	return nil
}

func (nd *NodesModel) GetAllNode(db *db.DBClient) ([]NodesModel, error) {
	storedJSONData, err := db.Get(nd.TableName())
	if err != nil {
		return nil, err
	}

	var storedData []NodesModel
	err = json.Unmarshal([]byte(storedJSONData), &storedData)
	if err != nil {
		return nil, err
	}

	return storedData, nil
}

func (nd *NodesModel) ClearNodes(db *db.DBClient) error {
	err := db.Del(nd.TableName())
	if err != nil {
		return err
	}
	return nil
}

//func (nd *NodesModel) AddNode(db *db.DBClient) error {
//	nodeData := structs.Map(nd)
//	return db.MultiHSet(nd.TableName(), nodeData)
//}
//
//func (nd *NodesModel) DelKeyNode(db *db.DBClient, key string) error {
//	return db.HDel(nd.TableName(), key)
//}
//
//func (nd *NodesModel) GetNodeInfo(db *db.DBClient) (*NodesModel, error) {
//	data, err := db.HGetAll(nd.TableName())
//	if err != nil {
//		return nil, err
//	}
//
//	fmt.Sprintf("dox %+v", data)
//	return nd, nil
//}
//
//func (nd *NodesModel) ClearNode(db *db.DBClient) error {
//	return db.HClear(nd.TableName())
//}
