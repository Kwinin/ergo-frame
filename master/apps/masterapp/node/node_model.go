package node

import (
	"encoding/json"
	"master/common"
	"master/db"
	"master/log"
)

type NodesModel struct {
	Id        int32
	Role      string
	Name      string
	Addr      string
	Status    int
	GenServer string
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

func (nd *NodesModel) AppendOneNode(db *db.DBClient, newNode NodesModel) error {
	nodes, err := nd.GetAllNode(db)

	n := make([]NodesModel, 0)
	for _, oldV := range nodes {
		if newNode.GenServer == oldV.GenServer && newNode.Addr == oldV.Addr {
			log.Logger.Warnf("the same value %d/%s,%s Status %d in DB", oldV.Id, oldV.GenServer, oldV.Addr, oldV.Status)
			oldV.Status = common.Online
		}
		n = append(n, oldV)
	}
	jsonData, err := json.Marshal(n)
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

func (nd *NodesModel) UpdateStatusNode(db *db.DBClient, genServer string, addr string, status int) error {
	nodes, err := nd.GetAllNode(db)
	n := make([]NodesModel, 0)
	for _, oldV := range nodes {
		if genServer == oldV.GenServer && addr == oldV.Addr {
			oldV.Status = status
		}
		n = append(n, oldV)
	}

	jsonData, err := json.Marshal(n)
	if err != nil {
		return err
	}
	db.Set(nd.TableName(), string(jsonData))
	return nil
}

func (nd *NodesModel) GetNodesByStatus(db *db.DBClient, status int) ([]NodesModel, error) {
	nodes, err := nd.GetAllNode(db)
	if err != nil {
		return nil, err
	}
	n := make([]NodesModel, 0)
	for _, oldV := range nodes {
		if oldV.Status == status {
			n = append(n, oldV)
		}
	}
	return n, nil
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
