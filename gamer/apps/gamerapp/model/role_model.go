package model

import (
	"fmt"
	"gamer/db"
	"github.com/fatih/structs"
)

type RoleModel struct {
	PlayerId      int
	GenServerName string
	Status        int
}

func newRoleModel() *RoleModel {
	nd := new(RoleModel)
	return nd
}

func NewRoleModel() *RoleModel {
	return newRoleModel()
}

func (r *RoleModel) TableName() string {
	return fmt.Sprintf("role_model:%d", r.PlayerId)
}

func (r *RoleModel) AddRole(db *db.DBClient) error {
	nodeData := structs.Map(r)
	return db.MultiHSet(r.TableName(), nodeData)
}

func (r *RoleModel) GetRoleInfo(db *db.DBClient) (*RoleModel, error) {
	data, err := db.HGetAll(r.TableName())
	if err != nil {
		return nil, err
	}

	r.PlayerId = data["PlayerId"].Int()
	r.GenServerName = data["GenServerName"].String()
	r.Status = data["Status"].Int()

	return r, nil
}

func (r *RoleModel) ClearRole(db *db.DBClient) error {
	return db.HClear(r.TableName())
}
