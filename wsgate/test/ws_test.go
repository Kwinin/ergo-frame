package test

import (
	"fmt"
	"github.com/gorilla/websocket"
	"log"
	"net/http"
	"net/url"
	"testing"
)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func handleWebSocketConnection(w http.ResponseWriter, r *http.Request) {
	// Upgrade HTTP request to WebSocket connection
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println("Error upgrading connection:", err)
		return
	}
	defer conn.Close()

	for {
		// Read message from the client
		_, message, err := conn.ReadMessage()
		if err != nil {
			fmt.Println("Error reading message:", err)
			break
		}

		// Print the received message
		fmt.Printf("Received message: %s\n", message)

		// Send a response back to the client
		response := "This is the server response."
		err = conn.WriteMessage(websocket.TextMessage, []byte(response))
		if err != nil {
			fmt.Println("Error writing message:", err)
			break
		}
	}
}

func TestServer(t *testing.T) {
	http.HandleFunc("/ws", handleWebSocketConnection)
	err := http.ListenAndServe(":8081", nil)
	if err != nil {
		fmt.Println("Error starting WebSocket server:", err)
		return
	}
}

func TestClient(t *testing.T) {
	// Connect to the WebSocket server
	u := url.URL{Scheme: "ws", Host: "localhost:8080", Path: "/ws"}
	conn, _, err := websocket.DefaultDialer.Dial(u.String(), nil)
	if err != nil {
		log.Fatal("Error connecting:", err)
	}
	defer conn.Close()

	// Send a message to the server
	message := "Hello, server! This is the client."
	err = conn.WriteMessage(websocket.TextMessage, []byte(message))
	if err != nil {
		log.Fatal("Error writing message:", err)
	}

	// Receive and print the response from the server
	_, response, err := conn.ReadMessage()
	if err != nil {
		log.Fatal("Error reading response:", err)
	}
	fmt.Println("Server response:", string(response))
}
