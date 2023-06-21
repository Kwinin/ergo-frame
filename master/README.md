## Project: "Master"

### Generated with
 - Types for network messaging: false
 - Enabled Cloud feature: false

### Supervision Tree

Applications
 - `MasterApp{}` master/apps/masterapp/masterapp.go
   - `MasterSup{}` master/apps/masterapp/mastersup.go
     - `MasterActor{}` master/apps/masterapp/masteractor.go


#### Used command
`ergo -init Master -with-app MasterApp -with-sup MasterApp:MasterSup{type:rfo,restart:trans} -with-actor MasterSup:MasterActor`
