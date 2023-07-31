package state

import (
	"fmt"
	"testing"
	"wsgate/apps/wsgateapp/db"
)

func TestGetAllState(t *testing.T) {
	client, err := db.NewDBClient("127.0.0.1", 8888)
	if err != nil {
		t.Fatalf("无法连接到SSDB数据库：%v", err)
	}
	defer client.Close()
	store, err := GetAllState(client)
	if err != nil {
		fmt.Errorf("err: %s", err)
	}
	fmt.Printf("kwinin %+v", store)
}

func TestStateModel_AddState(t *testing.T) {
	client, err := db.NewDBClient("127.0.0.1", 8888)
	if err != nil {
		t.Fatalf("无法连接到SSDB数据库：%v", err)
	}
	defer client.Close()
	sta := NewStateModel()
	sta.Pid = "3434"
	sta.PlayerId = "111"
	sta.Status = 1
	sta.AddState(client)
}
