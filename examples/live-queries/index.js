var express = require("express");
var ParseServer = require("parse-server").ParseServer;
var path = require("path");

var api = new ParseServer({
  databaseURI: "mongodb://localhost:27017",
  cloud: "./cloud/main.js",
  appId: "test",
  masterKey: "test",
  serverURL: "http://localhost:1337/parse",
  restApiKey: "test",
  liveQuery: {
    classNames: ["_User"]
  },
  verbose: false
});
var app = express();
app.use("/parse", api);
var httpServer = require("http").createServer(app);
httpServer.listen(1337, function() {});
ParseServer.createLiveQueryServer(httpServer);
