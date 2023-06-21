## Project: "Gamer"

### Generated with
 - Types for network messaging: false
 - Enabled Cloud feature: false

### Supervision Tree

Applications
 - `MyApp{}` gamer/apps/myapp/myapp.go
   - `GamerSup{}` gamer/apps/myapp/gamersup.go
     - `GamerActor{}` gamer/apps/myapp/gameractor.go
     - `PlayerSup{}` gamer/apps/myapp/playersup.go
	   - ... has more items. See source code


#### Used command
`ergo -init Gamer -with-app GamerApp -with-sup GamerApp:GamerSup -with-actor GamerSup:GamerActor -with-sup GamerSup:PlayerSup -with-actor PlayerSup:PlayerActor`
