var express = require("express");
var ParseServer = require("parse-server").ParseServer;
var path = require("path");

var api = new ParseServer({
  databaseURI: "mongodb://localhost:27017/",
  cloud: "./cloud/main.js",
  appId: "test",
  masterKey: "test",
  serverURL: "http://localhost:1337/parse",
  liveQuery: {
    classNames: ["_User", "Event"]
  },
  verbose: false
});

var app = express();
app.use("/parse", api);
var port = 1337;
var httpServer = require("http").createServer(app);
httpServer.listen(port, function() {
  console.log("Hello World!");
});
ParseServer.createLiveQueryServer(httpServer);
