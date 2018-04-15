var express = require("express");
var ParseServer = require("parse-server").ParseServer;
var path = require("path");

var api = new ParseServer({
  databaseURI: "mongodb://localhost:27017",
  serverURL: "http://localhost:1337/parse",
  appId: "test",
  masterKey: "test",
  restApiKey: "test",
  liveQuery: {
    classNames: ["_User"]
  },
  cloud: "./cloud/main.js",
  verbose: false
});
var app = express();
app.use("/parse", api);
var httpServer = require("http").createServer(app);
httpServer.listen(1337, function() {});
ParseServer.createLiveQueryServer(httpServer);
