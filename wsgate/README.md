## Project: "WsGate"

### Generated with
 - Types for network messaging: false
 - Enabled Cloud feature: false

### Supervision Tree

Applications
 - `WsGateApp{}` wsgate/apps/wsgateapp/wsgateapp.go
   - `WsGateSup{}` wsgate/apps/wsgateapp/wsgatesup.go
     - `WsGateActor{}` wsgate/apps/wsgateapp/wsgateactor.go


#### Used command
`ergo -init WsGate -with-app WsGateApp -with-sup WsGateApp:WsGateSup -with-actor WsGateSup:WsGateActor`
