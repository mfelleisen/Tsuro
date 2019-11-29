## The Server Start-up Protocol 

```
server(ip) : ip for IP address
 |
 |<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ tcp connection 1
 |
 | new(in,out)    remote_player = rp1
 |----------------->+
 |                  |
 . . . . . . . . . .|
 |                  |
 |<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ tcp connection 2
 |                  |
 |                  |
 | new(in,out)      |     remote_player = rp2
 |-------------------------->+
 |                  |        |
 . . . . . . . . . .|........|
 |                  |        |
 . . . . . . . . . .|........|
 |                  |        |
 | new(rp1,rp2)     |        |  create a new tournament manager
 |------------->+   |        |  with non-empty sequence of 
 |              |   |        |  remote players 
 | run_tour()   |   |        |
 |------------->|   |        |
```

## The Client Start-up Protocol 

```
client(ip,p) : ip for IP address, p for Port 
 |
 | new()  player = p
 |------->+
 |        |
 |~~~~~~~~~~~~~~~~~~~~~~~~~~> tcp connection to ip at p
 |        |
 |  new(in,out)   remote_admim
 |----------------->+
 |        |         |
 | run(p) |         |
 |----------------->|
 |        |         |
```
